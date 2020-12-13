Public Class MapGenWrapper

    Private objPlace As ImpenetrableObjects
    Dim genmesh As New ImpenetrableMeshGen
    Dim watergenerator As New WaterGen
    Dim stackstats As New StackPowerGen
    Dim racegen As New RaceGen
    Dim penOnjGen As New PenetrableObjectsGen

    Public Structure Parameter
        ''' <summary>Название параметра</summary>
        Public name As String
        ''' <summary>Тип значения</summary>
        Public type As ValueType

        ''' <summary>Минимальное значение, если это не массив</summary>
        Public minValue As String
        ''' <summary>Максимальное значение, если это не массив</summary>
        Public maxValue As String

        ''' <summary>Возможные наборы параметров, если тип значения - массив. Можно выбирать только из одного наборы.
        ''' Например: если что-то выбрано из possibleArrayValues(0), то нельзя брать из possibleArrayValues(1)</summary>
        Public possibleArrayValues()() As String

        Enum ValueType
            vStringArray = 1
            vDouble = 2
            vInteger = 3
            vBoolean = 4
        End Enum

        Friend Const wArray As String = "[StringArray]"
        Friend Const wDouble As String = "[Double]"
        Friend Const wInteger As String = "[Integer]"
        Friend Const wBoolean As String = "[Boolean]"

    End Structure
    Public Shared Function GetPermissibleParametersRange() As Parameter()
        Dim r() As String = ValueConverter.TxtSplit(My.Resources.GenParametersRange)
        Dim test() As String = ValueConverter.TxtSplit(My.Resources.example_template_1)

        Dim res(UBound(r)) As Parameter
        Dim nameField As Integer = 0
        Dim typeField As Integer = 1
        Dim minField As Integer = 2
        Dim maxField As Integer = 3

        Dim splR(UBound(r))() As String
        Dim testExampleNames, testParametersNames As New List(Of String)
        For i As Integer = 0 To UBound(r) Step 1
            splR(i) = r(i).Split(CChar(" "))
            Dim t As String = splR(i)(nameField).ToUpper
            If Not testParametersNames.Contains(t) Then
                testParametersNames.Add(t)
            Else
                Throw New Exception("Дублирование параметра " & splR(i)(nameField) & " в файле с доиустимыми значениями (GenParametersRange.txt)")
            End If
        Next i
        For i As Integer = 1 To UBound(test) Step 1
            Dim t As String = test(i).Split(CChar(" "))(nameField).ToUpper
            If Not t.StartsWith(My.Resources.template_new_Block) Then
                If Not testExampleNames.Contains(t) Then testExampleNames.Add(t)
            End If
        Next i
        For Each t As String In testParametersNames
            If Not testExampleNames.Contains(t) Then Throw New Exception("Не могу найти параметр " & t & " в тестовом шаблоне (example_template_1)")
        Next t
        For Each t As String In testExampleNames
            If Not testParametersNames.Contains(t) Then Throw New Exception("Не могу найти параметр " & t & " в файле с доиустимыми значениями (GenParametersRange.txt)")
        Next t

        For i As Integer = 0 To UBound(r) Step 1
            res(i).name = splR(i)(nameField)
            If splR(i)(typeField).ToUpper.StartsWith(Parameter.wArray.ToUpper) Then
                res(i).type = Parameter.ValueType.vStringArray
            ElseIf splR(i)(typeField).ToUpper.StartsWith(Parameter.wDouble.ToUpper) Then
                res(i).type = Parameter.ValueType.vDouble
            ElseIf splR(i)(typeField).ToUpper.StartsWith(Parameter.wInteger.ToUpper) Then
                res(i).type = Parameter.ValueType.vInteger
            ElseIf splR(i)(typeField).ToUpper.StartsWith(Parameter.wBoolean.ToUpper) Then
                res(i).type = Parameter.ValueType.vBoolean
            Else
                Throw New Exception("Неожиданный тип переменной: " & splR(i)(1) & " параметра " & splR(i)(0))
            End If
            If Not res(i).type = Parameter.ValueType.vStringArray Then
                res(i).minValue = splR(i)(minField)
                res(i).maxValue = splR(i)(maxField)
            Else
                Dim delimiters As String = splR(i)(typeField).Substring(Parameter.wArray.Length)
                Dim d1 As Char = delimiters(0)
                Dim d2 As Char = delimiters(1)
                Dim arrays() As String = splR(i)(minField).Split(d1)
                ReDim res(i).possibleArrayValues(UBound(arrays))
                For j As Integer = 0 To UBound(arrays) Step 1
                    res(i).possibleArrayValues(j) = arrays(j).Replace("[", "").Replace("]", "").Split(d2)
                Next j
            End If
        Next i
        Return res
    End Function

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

        Dim PlayersRaces() As Integer = Nothing
        If Not IsNothing(settGen.common_settMap.PlayersRaces) Then
            'If Not settGen.common_settMap.PlayersRaces.Length = settGen.common_settMap.nRaces Then
            '    Throw New Exception("Список рас не соответствует заданному количеству в nRaces")
            '    Return Nothing
            'End If
            Dim ok As Boolean
            ReDim PlayersRaces(UBound(settGen.common_settMap.PlayersRaces))
            For i As Integer = 0 To UBound(settGen.common_settMap.PlayersRaces) Step 1
                If settGen.common_settMap.PlayersRaces(i).ToUpper = GenDefaultValues.wTemplate_RandomRaceLongKeyword.ToUpper Or _
                   settGen.common_settMap.PlayersRaces(i).ToUpper = GenDefaultValues.wTemplate_RandomRaceShortKeyword.ToUpper Then
                    PlayersRaces(i) = GenDefaultValues.randomRaceID
                Else
                    PlayersRaces(i) = genmesh.comm.RaceIdentifierToSubrace(settGen.common_settMap.PlayersRaces(i))
                    ok = False
                    For Each R As String In genmesh.comm.defValues.playableRaces
                        If PlayersRaces(i) = genmesh.comm.RaceIdentifierToSubrace(R) Then
                            ok = True
                            Exit For
                        End If
                    Next R
                    If Not ok Then
                        Throw New Exception("Раса " & settGen.common_settMap.PlayersRaces(i) & " не является играбельной")
                        Return Nothing
                    End If
                    For j As Integer = 0 To i - 1 Step 1
                        If PlayersRaces(i) = PlayersRaces(j) Then
                            Throw New Exception("В списке рас игроков присутствуют две одинаковые расы")
                            Return Nothing
                        End If
                    Next j
                End If
            Next i
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
            Call racegen.Gen(grid, PlayersRaces, copiedSettings)
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