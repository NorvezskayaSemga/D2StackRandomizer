Public Class Loader

    '  char(getName) (void);
    ' TemplateList(getTemplates) (void);
    ' GenerationSettings(getSettings) (const Template&);
    ' ErrorCode(generateMap) (const char, const DbfData&, const GenerationSettings&);
    '
    '
    ' Public Function getName() As Char()
    '     Return "Semga's generator 30.07.2020".ToCharArray
    ' End Function


End Class

Public Class MapGenWrapper

    Private objPlace As ImpenetrableObjects
    Dim genmesh As New ImpenetrableMeshGen
    Dim watergenerator As New WaterGen
    Dim stackstats As New StackPowerGen
    Dim racegen As New RaceGen
    Dim penOnjGen As New PenetrableObjectsGen

    ''' <param name="ImpObjectPlacer">Инициализированный класс</param>
    Public Sub New(ByRef ImpObjectPlacer As ImpenetrableObjects)
        If IsNothing(ImpObjectPlacer) Then Throw New Exception("Инициализируй ImpObjectPlacer, блять")
        objPlace = ImpObjectPlacer
    End Sub

    ''' <summary>Генерирует заготовку ландшафта. В случае неудачи вернет пустую карту с сохраненным логом</summary>
    ''' <param name="sM">Общие настройки для карты</param>
    ''' <param name="sR">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="sC">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт</param>
    ''' <param name="genTimeLimit">Максимальное время на операцию расстановки объектов в миллисекундах.
    ''' Она обычно производится меньше чем за пару секунд, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту.
    ''' Если не получится с пяти попыток, вернет пустую карту с сохраненным логом</param>
    Public Function SimpleGen(ByRef sM As Map.SettingsMap, _
                              ByRef sR As Map.SettingsLoc, _
                              ByRef sC As Map.SettingsLoc, _
                              ByVal genTimeLimit As Integer) As Map

        Dim sett As New ImpenetrableMeshGen.GenSettings
        sett.genMode = ImpenetrableMeshGen.GenSettings.genModes.simple
        sett.common_settMap = sM
        sett.simple_settRaceLoc = sR
        sett.simple_settCommLoc = sC
        Return CommonGen(sett, genTimeLimit)
    End Function

    ''' <summary>Генерирует заготовку ландшафта. В случае неудачи вернет пустую карту с сохраненным логом</summary>
    ''' <param name="sM">Общие настройки для карты</param>
    ''' <param name="sL">Настройки для каждой локации. Первыми должны идти стартовые локации рас.
    ''' Коментарий к настройкам стартовых локаций играбельных рас:
    ''' дробная часть определяет шанс округления большую сторону.
    ''' Комментарий к настройкам остальных локаций:
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади, заданной в настройках (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт </param>
    ''' <param name="genTimeLimit">Максимальное время на операцию расстановки объектов в миллисекундах.
    ''' Она обычно производится меньше чем за пару секунд, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту.
    ''' Если не получится с пяти попыток, вернет пустую карту с сохраненным логом</param>
    Public Function TemplateGen(ByRef sM As Map.SettingsMap, _
                                ByRef sL() As ImpenetrableMeshGen.GenSettings.LocationGenSetting, _
                                ByVal genTimeLimit As Integer) As Map
        Dim sett As New ImpenetrableMeshGen.GenSettings
        sett.genMode = ImpenetrableMeshGen.GenSettings.genModes.template
        sett.common_settMap = sM
        sett.template_settGenLoc = sL
        Return CommonGen(sett, genTimeLimit)
    End Function

    ''' <summary>Генерирует заготовку ландшафта. В случае неудачи вернет пустую карту с сохраненным логом</summary>
    ''' <param name="templates">Шаблоны, из которых случайным образом будет выбран один</param>
    ''' <param name="genTimeLimit">Максимальное время на операцию расстановки объектов в миллисекундах.
    ''' Она обычно производится меньше чем за пару секунд, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту.
    ''' Если не получится с пяти попыток, вернет пустую карту с сохраненным логом</param>
    Public Function TemplatePoolGen(ByRef templates() As ImpenetrableMeshGen.GenSettings, _
                                    ByVal genTimeLimit As Integer) As Map
        Dim selected As Integer = (New RndValueGen).RndInt(0, UBound(templates), True)
        Return CommonGen(templates(selected), genTimeLimit)
    End Function

    ''' <summary>Генерирует заготовку ландшафта. В случае неудачи вернет пустую карту с сохраненным логом</summary>
    ''' <param name="settGen">Если настройки прочтены из файла, генератор сам разберется, для какого они режима</param>
    ''' <param name="genTimeLimit">Максимальное время на операцию расстановки объектов в миллисекундах.
    ''' Она обычно производится меньше чем за пару секунд, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту.
    ''' Если не получится с пяти попыток, вернет пустую карту с сохраненным логом</param>
    Public Function CommonGen(ByRef settGen As ImpenetrableMeshGen.GenSettings, _
                              ByVal genTimeLimit As Integer) As Map

        Dim t0 As Integer = Environment.TickCount

        Dim grid As New Map(0, 0, -1, New Common)
        Dim copiedSettings() As Map.SettingsLoc

        Dim invalidSettings As Boolean = False
        Dim checkResult As String
        checkResult = settGen.common_settMap.Check
        If checkResult.Length > 0 Then
            invalidSettings = True
            Call grid.log.Add("Invalid settings detected in Common Map settings:")
            Call grid.log.Add(checkResult)
        End If

        If settGen.genMode = ImpenetrableMeshGen.GenSettings.genModes.simple Then
            checkResult = settGen.simple_settRaceLoc.Check
            If checkResult.Length > 0 Then
                invalidSettings = True
                Call grid.log.Add("Invalid settings detected in Race Locations settings:")
                Call grid.log.Add(checkResult)
            End If
            checkResult = settGen.simple_settCommLoc.Check
            If checkResult.Length > 0 Then
                invalidSettings = True
                Call grid.log.Add("Invalid settings detected in Common Locations settings:")
                Call grid.log.Add(checkResult)
            End If
        ElseIf settGen.genMode = ImpenetrableMeshGen.GenSettings.genModes.template Then
            'For i As Integer = 0 To UBound(settGen.template_settLoc) Step 1
            '    checkResult = settGen.template_settLoc(i).Check
            '    If checkResult.Length > 0 Then Console.WriteLine(checkResult)
            'Next i
        Else
            Throw New Exception("Unknown gen mode")
            Return Nothing
        End If
        If invalidSettings Then
            grid.Clear()
            Return grid
        End If

        Dim settBak As ImpenetrableMeshGen.GenSettings = ImpenetrableMeshGen.GenSettings.Copy(settGen)
again:
        settGen = ImpenetrableMeshGen.GenSettings.Copy(settBak)
        grid = genmesh.GenMap(settGen, genTimeLimit, grid.log)

        If IsNothing(grid.board) Then
            grid.Clear()
            Return grid
        ElseIf Not grid.TestMap = "" Then
            Call grid.log.Add(grid.TestMap)
            grid.Clear()
            Return grid
        End If
        

        If settGen.genMode = ImpenetrableMeshGen.GenSettings.genModes.simple Then
            copiedSettings = Map.SettingsLoc.ToArray(settGen.simple_settRaceLoc, _
                                                     settGen.simple_settCommLoc, _
                                                     settGen.common_settMap.nRaces, _
                                                     grid.Loc.Length)
        ElseIf settGen.genMode = ImpenetrableMeshGen.GenSettings.genModes.template Then
            copiedSettings = Map.SettingsLoc.Copy(settGen.template_settLoc)
        Else
            copiedSettings = Nothing
        End If

        If Not IsNothing(grid.board) Then
            Dim staclocgen As New StackLocationsGen(genmesh)
            If Not staclocgen.Gen(grid, settGen.common_settMap, copiedSettings, genTimeLimit) Then GoTo again
        Else
            grid.Clear()
            Return grid
        End If

        If Not grid.TestMap = "" Then
            Call grid.log.Add(grid.TestMap)
            grid.Clear()
            Return grid
        End If

        Try
            Call stackstats.Gen(grid, settGen.common_settMap, copiedSettings)
            Call watergenerator.Gen(grid, settGen.common_settMap)
            Call racegen.Gen(grid, Nothing, copiedSettings)
            Call objPlace.Gen(grid, settGen.common_settMap, copiedSettings)
            Call penOnjGen.Gen(grid, settGen.common_settMap)
        Catch ex As Exception
            grid.log.Add(ex.Message & vbNewLine & ex.StackTrace)
            GoTo again
        End Try

        Call grid.log.Add(vbNewLine & "Total generation time: " & Environment.TickCount - t0 & " ms")

        If Not settBak.common_settMap.xSize = grid.xSize + 1 Or Not settBak.common_settMap.ySize = grid.ySize + 1 Then
            Throw New Exception("Map size is not consistent with desired size")
        End If

        Return grid
    End Function


End Class