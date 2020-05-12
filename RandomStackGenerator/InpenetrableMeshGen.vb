Imports System.Drawing
Imports System.ComponentModel
Imports System.Threading.Tasks

Public Class ImpenetrableMeshGen

    Public ReadOnly minLocationRadiusAtAll As Double
    Public Sub New()
        minLocationRadiusAtAll = (New ValueConverter).minLocationRadiusAtAll
    End Sub

    Private Structure PrepareToRaceLocGenResult
        Dim DminimumDist As Double
        ''' <summary>Минимальное расстояние^2</summary>
        Dim IminimumDist As Integer
        Dim possiblePoints() As Point
        Dim ppIDs As List(Of Integer)
        Dim raceLocs() As Location
        Dim equalDist(,) As List(Of Integer)
    End Structure

    Private rndgen As New RndValueGen
    Private comm As New Common
    Private symm As New SymmetryOperations

    Public ActiveObjects() As AttendedObject = New AttendedObject() {Nothing, _
                                               New AttendedObject("Capital", 5, 1), _
                                               New AttendedObject("City", 4, 2), _
                                               New AttendedObject("Vendor", 3, 3, True), _
                                               New AttendedObject("Mercenary", 3, 4, True), _
                                               New AttendedObject("Mage", 3, 5, True), _
                                               New AttendedObject("Trainer", 3, 6, True), _
                                               New AttendedObject("Ruins", 3, 7), _
                                               New AttendedObject("Mine", 1, 8)}

    Private Function ActiveObjectsSet(ByRef settMap As Map.SettingsMap, ByRef symmId As Integer) As Map.Cell()(,)
        Dim symm As New SymmetryOperations
        Dim result(UBound(ActiveObjects))(,) As Map.Cell
        For i As Integer = 1 To UBound(ActiveObjects) Step 1
            Dim r As Integer = ActiveObjects(i).Size + 2 * ActiveObjects(i).dxy - 1
            Dim d As Integer = ActiveObjects(i).dxy
            ReDim result(i)(r, r)

            For y As Integer = d To r - d Step 1
                For x As Integer = d To r - d Step 1
                    result(i)(x, y).isAttended = True
                Next x
            Next y

            For y As Integer = r - d - 1 To r - d + 1 Step 1
                For x As Integer = r - d - 1 To r - d + 1 Step 1
                    If Not result(i)(x, y).isAttended Then
                        result(i)(x, y).Penetrable = True
                    End If
                Next x
            Next y
            If ActiveObjects(i).hasExternalGuard Then
                For y As Integer = r - d To r - d + 2 Step 1
                    For x As Integer = r - d To r - d + 2 Step 1
                        If Not result(i)(x, y).isAttended Then
                            result(i)(x, y).Penetrable = True
                        End If
                    Next x
                Next y
                For k As Integer = -1 To 1 Step 2
                    result(i)(r - d + k, r - d - k).Penetrable = False
                    result(i)(r - d + k, r - d - k).isBorder = True
                Next k
                result(i)(r - d + 1, r - d + 1).GuardLoc = True
            End If

            If symmId > -1 Then
                Dim p() As Point
                For y As Integer = 0 To r Step 1
                    For x As Integer = 0 To r Step 1
                        p = symm.ApplySymm(New Point(x, y), settMap.nRaces, New Map(r, r, symmId), 1)
                        For k As Integer = 0 To UBound(p) Step 1
                            result(i)(p(k).X, p(k).Y).isAttended = result(i)(x, y).isAttended Or result(i)(p(k).X, p(k).Y).isAttended
                            result(i)(p(k).X, p(k).Y).Penetrable = result(i)(x, y).Penetrable Or result(i)(p(k).X, p(k).Y).Penetrable
                            result(i)(p(k).X, p(k).Y).isBorder = result(i)(x, y).isBorder Or result(i)(p(k).X, p(k).Y).isBorder
                        Next k
                    Next x
                Next y
            End If
            result(i)(d, d).objectID = i
        Next i
        Return result
    End Function

    Private Function CommonGen(ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, _
                               ByRef settCommLoc As Map.SettingsLoc, ByRef maxGenTime As Integer, _
                               ByRef symmId As Integer) As Map

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")
        If Not settRaceLoc.isChecked Then Throw New Exception("Check parameters via settRaceLoc.Check()")
        If Not settCommLoc.isChecked Then Throw New Exception("Check parameters via settCommLoc.Check()")

        Dim AObj()(,) As Map.Cell = ActiveObjectsSet(settMap, symmId)
        Dim term As New TerminationCondition(maxGenTime)
        Dim AttemptsN = 0
        Dim m As Map = Nothing
        Dim nTry As Integer = 0
        Do While AttemptsN < 5
            Try
                Dim t0 As Integer = Environment.TickCount
                m = PlaceRaceLocations(settMap, settRaceLoc, symmId)
                Dim t1 As Integer = Environment.TickCount
                Call PlaceCommonLocs(m, settMap, settCommLoc)
                Dim t2 As Integer = Environment.TickCount
                Call SetLocIdToCells(m, settMap)
                Dim t3 As Integer = Environment.TickCount
                Call SetBorders(m, settMap, term)
                Dim t4 As Integer = Environment.TickCount
                Call PlaceActiveObjects(m, settMap, settRaceLoc, settCommLoc, AObj, term)
                Dim t5 As Integer = Environment.TickCount
                Call MakeLabyrinth(m, settMap, term)
                Dim t6 As Integer = Environment.TickCount
                Console.WriteLine("ReaceLocs: " & t1 - t0 & vbTab & "CommonLocs: " & t2 - t1 & vbTab & "IDset: " & t3 - t2 & vbTab & "BordSet: " & t4 - t3 & vbTab & "PlaceActiveObjects: " & t5 - t4 & vbTab & "MakeMaze: " & t6 - t5)
                AttemptsN += 1
                If Not term.ExitFromLoops Then
                    m.complited.LoationsCreation_Done = True
                    Return m
                End If
                nTry = 0
            Catch ex As Exception
                If nTry > 1 Then
                    Throw ex
                Else
                    Console.WriteLine("Some error occured in ImpenetrableMeshGen: " & vbNewLine & ex.Message)
                    nTry += 1
                End If
            End Try
        Loop
        Return Nothing
    End Function
    ''' <summary>Генерирует заготовку ландшафта без использования симметрии</summary>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт</param>
    ''' <param name="maxGenTime">Максимальное время на операцию расстановки объектов.
    ''' Она обычно производится меньше чем за пару секунд, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту.
    ''' Если не получится с пяти попыток, вернет Nothing</param>
    Public Function UnsymmGen(ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, _
                              ByRef settCommLoc As Map.SettingsLoc, ByRef maxGenTime As Integer) As Map
        Return CommonGen(settMap, settRaceLoc, settCommLoc, maxGenTime, -1)
    End Function
    ''' <summary>Генерирует заготовку ландшафта с использованием симметрии</summary>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону. В случае округления вниз дробная часть добавляется к максимальному количеству шахт</param>
    ''' <param name="maxGenTime">Максимальное время на операцию расстановки объектов.
    ''' Она обычно производится меньше чем за пару секунд, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту.
    ''' Если не получится с пяти попыток, вернет Nothing</param>
    ''' <param name="symmID">ID применяемой операпции симметрии (см. класс SymmetryOperations).
    ''' Если ID меньше ноля, будет выбрана случайная симметрия из тех, что подходят</param>
    Public Function SymmGen(ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, _
                            ByRef settCommLoc As Map.SettingsLoc, ByRef maxGenTime As Integer, _
                            Optional ByRef symmID As Integer = -1) As Map
        Dim s As Integer
        Dim slist As List(Of Integer) = symm.PossibleOperationsList(settMap.nRaces, settMap.xSize, settMap.ySize)
        If symmID > -1 Then
            s = symmID
            If Not slist.Contains(s) Then
                MsgBox("Я, конечно, попробую, но вообще выбранная симметрия не подходит под выбранные параметры карты")
            End If
        Else
            If IsNothing(slist) OrElse slist.Count = 0 Then Throw New Exception("Должно быть две или четыре расы")
            s = comm.RandomSelection(slist, True)
        End If
        slist = Nothing
        Return CommonGen(settMap, settRaceLoc, settCommLoc, maxGenTime, s)
    End Function

    Private Sub PlaceLoc(ByRef m As Map, ByRef loc As Location)
        Dim b As Location.Borders = loc.XYborders(UBound(m.board, 1), UBound(m.board, 2))
        Dim plist As New List(Of Point)
        For x As Integer = b.minX To b.maxX Step 1
            For y As Integer = b.minY To b.maxY Step 1
                If loc.IsInside(x, y) Then
                    m.board(x, y).locID.Add(loc.ID)
                    plist.Add(New Point(x, y))
                End If
            Next y
        Next x
        For Each p As Point In plist
            b = NearestXY(p, m, 2)
            For x As Integer = b.minX To b.maxX Step 1
                For y As Integer = b.minY To b.maxY Step 1
                    If m.board(x, y).locID.Count = 0 Then
                        m.board(p.X, p.Y).isBorder = True
                        x = b.maxX
                        Exit For
                    End If
                Next y
            Next x
        Next p
    End Sub
    Friend Shared Function NearestXY(ByRef x As Integer, ByRef y As Integer, _
                                     ByRef xSize As Integer, ByRef ySize As Integer, _
                                     ByRef tolerance As Integer) As Location.Borders
        Return New Location.Borders With {.minx = Math.Max(x - tolerance, 0), _
                                          .maxx = Math.Min(x + tolerance, xSize), _
                                          .miny = Math.Max(y - tolerance, 0), _
                                          .maxy = Math.Min(y + tolerance, ySize)}
    End Function
    Friend Shared Function NearestXY(ByRef P As Point, ByRef M As Map, ByRef tolerance As Integer) As Location.Borders
        Return NearestXY(P.X, P.Y, M.xSize, M.ySize, tolerance)
    End Function

    Private Function PlaceRaceLocations(ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, ByRef symmID As Integer) As Map
        Dim res As New Map(settMap.xSize, settMap.ySize, symmID)
        Dim ok As Boolean = False
        Dim raceLocs() As Location
        Dim prepResult As PrepareToRaceLocGenResult = PrepareToRaceLocGen(settMap, settRaceLoc)
        If res.symmID > -1 Then
            Dim id As Integer
            Dim L As Location
            raceLocs = Nothing
            Do While Not ok
                L = prepResult.raceLocs(0).Copy
                id = comm.RandomSelection(prepResult.ppIDs, True)
                L.pos = New Point(prepResult.possiblePoints(id).X, prepResult.possiblePoints(id).Y)
                raceLocs = symm.ApplySymm(L, settMap.nRaces, res, prepResult.IminimumDist)
                If raceLocs.Length = settMap.nRaces Then
                    ok = TestRaceLocations(raceLocs, settMap.nRaces, prepResult, settMap.RaceLocsDistTolerance)
                Else
                    ok = False
                End If
            Loop
        Else
            Dim tryagain As Boolean = False
            Dim borderPoints() As Point = Nothing
            Dim id, t As Integer
            raceLocs = prepResult.raceLocs
            Dim SelID, tmpID As New List(Of Integer)
            Do While Not ok
                id = comm.RandomSelection(prepResult.ppIDs, True)
                raceLocs(0).pos = New Point(prepResult.possiblePoints(id).X, prepResult.possiblePoints(id).Y)

                SelID.Clear()
                For Each i As Integer In prepResult.equalDist(raceLocs(0).pos.X, raceLocs(0).pos.Y)
                    SelID.Add(i)
                Next i
                For n As Integer = 1 To settMap.nRaces - 1 Step 1
                    t = comm.RandomSelection(SelID, True)
                    raceLocs(n).pos = New Point(prepResult.possiblePoints(t).X, prepResult.possiblePoints(t).Y)

                    If n < settMap.nRaces - 1 Then
                        tmpID.Clear()
                        For Each i As Integer In SelID
                            tmpID.Add(i)
                        Next i
                        For Each i As Integer In tmpID
                            If Not prepResult.equalDist(raceLocs(n).pos.X, raceLocs(n).pos.Y).Contains(i) Then SelID.Remove(i)
                        Next i
                        If SelID.Count = 0 Then
                            tryagain = True
                            Exit For
                        End If
                    End If
                Next n
                If tryagain Then
                    tryagain = False
                Else
                    ok = TestRaceLocations(raceLocs, settMap.nRaces, prepResult, settMap.RaceLocsDistTolerance)
                End If
            Loop
        End If
        res.Loc = raceLocs
        For i As Integer = 0 To UBound(res.Loc) Step 1
            Call PlaceLoc(res, res.Loc(i))
        Next i
        Return res
    End Function
    Private Function PrepareToRaceLocGen(ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc) As PrepareToRaceLocGenResult
        Dim result As New PrepareToRaceLocGenResult
        ReDim result.raceLocs(settMap.nRaces - 1)
        For i As Integer = 0 To settMap.nRaces - 1 Step 1
            result.raceLocs(i) = Location.GenLocSize(settRaceLoc, i + 1, rndgen, minLocationRadiusAtAll)
        Next i

        If Math.Max(settMap.xSize, settMap.ySize) < 2 * settRaceLoc.AverageRadius + 10 Then Throw New Exception("Слишком большой радиус начальных локаций")
        If settRaceLoc.AverageRadius < 7 Then Throw New Exception("Слишком маленький радиус начальных локаций")

        Dim r, m As Double
        Dim n As Integer
        If settMap.nRaces = 4 Then
            r = 0.5 * Math.Sqrt(((settMap.xSize - 2 * settRaceLoc.AverageRadius) ^ 2) _
                               + ((settMap.ySize - 2 * settRaceLoc.AverageRadius) ^ 2) - 2)
            r = Math.Min(r, Math.Min(settMap.xSize, settMap.ySize) - 2)
        ElseIf settMap.nRaces = 3 Then
            r = 0.5 * Math.Sqrt(3) * (Math.Min(settMap.xSize, settMap.ySize) - 2 * settRaceLoc.AverageRadius)
        ElseIf settMap.nRaces = 2 Then
            r = 0.5 * (settMap.xSize + settMap.ySize - 4 * settRaceLoc.AverageRadius)
        Else
            Throw New Exception("Неожиданное аколичество рас: " & settMap.nRaces)
            r = 2 * settRaceLoc.AverageRadius + 2
        End If
        r = Math.Min(r, Math.Min(settMap.xSize, settMap.ySize) - 2)
        r = r * (1 - 0.5 * settMap.RaceLocsDistTolerance)
        If settMap.nRaces = 4 Then
            m = CInt(0.5 * Math.Min(r, Math.Min(settMap.xSize, settMap.ySize) - 2 * settRaceLoc.AverageRadius))
        ElseIf settMap.nRaces = 3 Then
            m = CInt(0.25 * Math.Sqrt(3) * r)
        ElseIf settMap.nRaces = 2 Then
            m = CInt(r / Math.Sqrt(2))
        Else
            m = 0
        End If

        result.DminimumDist = r
        result.IminimumDist = CInt((r - 2) ^ 2)

        Dim dX As Integer = CInt(Math.Min(0.5 * settMap.xSize - settRaceLoc.AverageRadius - 1, m))
        Dim dY As Integer = CInt(Math.Min(0.5 * settMap.ySize - settRaceLoc.AverageRadius - 1, m))
        Dim x1 As Integer = CInt(0.5 * settMap.xSize - dX)
        Dim x2 As Integer = CInt(0.5 * settMap.xSize + dX)
        Dim y1 As Integer = CInt(0.5 * settMap.ySize - dY)
        Dim y2 As Integer = CInt(0.5 * settMap.ySize + dY)

        result.ppIDs = New List(Of Integer)
        ReDim result.possiblePoints((settMap.xSize + 1) * (settMap.ySize + 1) - 1)
        n = -1
        For x As Integer = 0 To settMap.xSize Step 1
            If x >= settRaceLoc.AverageRadius And x <= settMap.xSize - settRaceLoc.AverageRadius Then
                For y As Integer = 0 To settMap.ySize Step 1
                    If y >= settRaceLoc.AverageRadius And y <= settMap.ySize - settRaceLoc.AverageRadius Then
                        If Not (x > x1 And x < x2 And y > y1 And y < y2) Then
                            n += 1
                            result.possiblePoints(n) = New Point(x, y)
                            result.ppIDs.Add(n)
                        End If
                    End If
                Next y
            End If
        Next x
        ReDim Preserve result.possiblePoints(n)

        Dim borderPoints(2 * (settMap.xSize + settMap.ySize) - 1) As Point
        n = -1
        For i As Integer = 0 To settMap.xSize Step 1
            n += 1
            borderPoints(n) = New Point(i, 0)
            n += 1
            borderPoints(n) = New Point(i, settMap.ySize)
        Next i
        For i As Integer = 1 To settMap.ySize - 1 Step 1
            n += 1
            borderPoints(n) = New Point(0, i)
            n += 1
            borderPoints(n) = New Point(settMap.xSize, i)
        Next i

        Dim distToBorder(settMap.xSize, settMap.ySize) As Integer
        ReDim result.equalDist(settMap.xSize, settMap.ySize)
        Parallel.For(0, result.possiblePoints.Length, _
         Sub(i As Integer)
             Dim k As Integer = 0
             For j As Integer = 0 To UBound(borderPoints) Step 1
                 k += result.possiblePoints(i).SqDist(borderPoints(j))
             Next j
             distToBorder(result.possiblePoints(i).X, result.possiblePoints(i).Y) = k
         End Sub)
        Parallel.For(0, result.possiblePoints.Length, _
         Sub(i As Integer)
             Dim k As Integer = distToBorder(result.possiblePoints(i).X, result.possiblePoints(i).Y)
             Dim dk As Integer = CInt(0.1 * k)
             Dim t As New List(Of Integer)
             Dim d As Integer
             For j As Integer = 0 To UBound(result.possiblePoints) Step 1
                 If Not i = j AndAlso Math.Abs(k - distToBorder(result.possiblePoints(j).X, result.possiblePoints(j).Y)) <= dk Then
                     d = result.possiblePoints(i).SqDist(result.possiblePoints(j))
                     If d >= result.IminimumDist Then t.Add(j)
                 End If
             Next j
             result.equalDist(result.possiblePoints(i).X, result.possiblePoints(i).Y) = t
         End Sub)

        Return result
    End Function
    Private Function TestRaceLocations(ByRef RLocs() As Location, ByRef nRaces As Integer, _
                                       ByRef prep As PrepareToRaceLocGenResult, _
                                       ByRef RTolerance As Double) As Boolean
        Dim D, minD1, minD2 As Integer
        For i As Integer = 0 To nRaces - 1 Step 1
            minD1 = -1
            For j As Integer = 0 To nRaces - 1 Step 1
                If Not j = i Then
                    D = RLocs(i).pos.SqDist(RLocs(j).pos)
                    If D < prep.IminimumDist Then Return False
                    If minD1 > -1 Then
                        If minD1 > D Then
                            minD2 = minD1
                            minD1 = D
                        ElseIf minD2 > D Then
                            minD2 = D
                        End If
                    Else
                        minD1 = D
                        minD2 = D
                    End If
                End If
            Next j
            If minD1 * Math.Pow(1 + RTolerance, 2) < minD2 Then Return False
        Next i
        Return True
    End Function

    Private Sub PlaceCommonLocs(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settCommLoc As Map.SettingsLoc)

        Dim id As Integer = m.Loc.Length + 1
        Dim dynRadiusDispersion As Double = settCommLoc.maxRadiusDispersion
        Dim dynAverageRadius As Double = settCommLoc.AverageRadius
        Dim dynBaseRadius As Double = settCommLoc.AverageRadius
        Dim possiblePoints((m.xSize + 1) * (m.ySize + 1) - 1) As Point
        Dim IDs As New List(Of Integer)
        Dim nextloop As Boolean = True

        Dim sLocs() As Location
        Do While nextloop
            Dim loc As Location = Location.GenLocSize(settCommLoc, id, rndgen, minLocationRadiusAtAll)
            'make possible ids
            Dim b As Location.Borders = loc.XYborders(Integer.MaxValue, Integer.MaxValue, Integer.MinValue, Integer.MinValue)
            Dim add As Boolean
            Dim locPlist As New List(Of Point)
            IDs.Clear()
            For x As Integer = b.minX To b.maxX Step 1
                For y As Integer = b.minY To b.maxY Step 1
                    If loc.IsInside(x, y) Then locPlist.Add(New Point(x, y))
                Next y
            Next x
            For x As Integer = 0 To m.xSize Step 1
                For y As Integer = 0 To m.ySize Step 1
                    add = False
                    For Each p As Point In locPlist
                        Dim dx As Integer = x + p.X
                        If dx >= 0 And dx <= m.xSize Then
                            Dim dy As Integer = y + p.Y
                            If dy >= 0 And dy <= m.ySize Then
                                If m.board(dx, dy).isBorder Then
                                    add = True
                                ElseIf m.board(dx, dy).locID.Count > 0 Then
                                    add = False
                                    Exit For
                                End If
                            End If
                        End If
                    Next p
                    If add Then
                        possiblePoints(IDs.Count) = New Point(x, y)
                        IDs.Add(IDs.Count)
                    End If
                Next y
            Next x
            '#################

            If IDs.Count > 0 Then
                Dim pid As Integer = comm.RandomSelection(IDs, True)
                loc.pos = New Point(possiblePoints(pid).X, possiblePoints(pid).Y)
                If m.symmID > -1 Then
                    Dim minR As Integer = CInt((0.9 * Math.Min(loc.gASize, loc.gBSize)) ^ 2)
                    sLocs = symm.ApplySymm(loc, settMap.nRaces, m, minR)
                    ReDim Preserve m.Loc(m.Loc.Length + UBound(sLocs))
                    For i As Integer = 0 To UBound(sLocs) Step 1
                        m.Loc(UBound(m.Loc) - UBound(sLocs) + i) = sLocs(i)
                        Call PlaceLoc(m, m.Loc(UBound(m.Loc) - UBound(sLocs) + i))
                    Next i
                    id = m.Loc.Length + 1
                Else
                    ReDim Preserve m.Loc(m.Loc.Length)
                    m.Loc(UBound(m.Loc)) = loc
                    Call PlaceLoc(m, m.Loc(UBound(m.Loc)))
                    id += 1
                End If
            Else
                nextloop = True
                'cange dyn parameters
                Dim newDisp As Double = 0.9 * dynRadiusDispersion
                If newDisp < 0.001 Then
                    dynRadiusDispersion = settCommLoc.maxRadiusDispersion
                    dynBaseRadius *= 0.9
                    dynAverageRadius = dynBaseRadius
                    If dynBaseRadius < 0.5 * settCommLoc.AverageRadius _
                    Or dynBaseRadius < minLocationRadiusAtAll Then nextloop = False
                Else
                    dynAverageRadius *= (1 - dynRadiusDispersion) / (1 - newDisp)
                    dynRadiusDispersion = newDisp
                End If
                '####################
            End If
        Loop
    End Sub

    Private Sub SetLocIdToCells(ByRef m As Map, ByVal settMap As Map.SettingsMap)

        Dim allPoints()() As Point = Nothing
        Dim pID()() As Integer = Nothing
        Dim selectedIDs() As Integer = Nothing
        Dim Weight()() As Double = Nothing
        Dim minweight As Double
        Dim selectedWeight() As Double = Nothing
        Dim calculatedWeights(,,) As Double = Nothing
        Dim idlist As List(Of Integer) = Nothing

        ReDim m.board(m.xSize, m.ySize)
        For x As Integer = 0 To m.xSize Step 1
            For y As Integer = 0 To m.ySize Step 1
                m.board(x, y).locID = New List(Of Integer)
                m.board(x, y).groupID = 0
                m.board(x, y).isAttended = False
                m.board(x, y).isBorder = False
                m.board(x, y).isPass = False
                m.board(x, y).objectID = 0
            Next y
        Next x
        For Each Loc As Location In m.Loc
            m.board(Loc.pos.X, Loc.pos.Y).locID.Add(Loc.ID)
        Next Loc
        ReDim allPoints(m.ySize)
        ReDim pID(UBound(allPoints)), Weight(UBound(allPoints)), _
              selectedIDs(UBound(allPoints)), selectedWeight(UBound(allPoints)), _
              calculatedWeights(m.xSize, m.ySize, UBound(m.Loc))
        minweight = 10 ^ -9
        For i As Integer = 0 To UBound(allPoints) Step 1
            ReDim Preserve allPoints(i)(m.xSize), pID(i)(m.xSize), Weight(i)(m.xSize)
        Next i
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                For i As Integer = 0 To UBound(m.Loc) Step 1
                    calculatedWeights(x, y, i) = -1
                Next i
            Next x
        Next y
        idlist = New List(Of Integer)
        idlist.Add(-1)

        If m.symmID > -1 Then
            Do While idlist.Count > 0
                Call makePointsList(m, idlist, allPoints, pID, Weight, minweight, selectedIDs, selectedWeight, calculatedWeights)
                If idlist.Count > 0 Then
                    Dim s As Integer = comm.RandomSelection(idlist, selectedWeight, True)
                    Dim pp() As Point = symm.ApplySymm(allPoints(s)(selectedIDs(s)), settMap.nRaces, m, 1)
                    If pp.Length > 1 Then
                        Dim pl() As Point = symm.ApplySymm(m.Loc(pID(s)(selectedIDs(s)) - 1).pos, settMap.nRaces, m, 1)
                        If pl.Length = pp.Length Then
                            For i As Integer = 0 To UBound(pl) Step 1
                                m.board(pp(i).X, pp(i).Y).locID.Add(m.Loc(Location.FindLocIDByPosition(m, pl(i))).ID)
                            Next i
                        ElseIf pl.Length = 1 Then
                            For i As Integer = 0 To UBound(pp) Step 1
                                m.board(pp(i).X, pp(i).Y).locID.Add(pID(s)(selectedIDs(s)))
                            Next i
                        Else
                            Dim possibleLocs As New List(Of Integer)
                            Dim usedLocs As New List(Of Integer)
                            For Each p As Point In pp
                                possibleLocs.Clear()
                                Dim t As Integer = 1
                                Do While t < Math.Max(m.xSize, m.ySize)
                                    Dim b As Location.Borders = NearestXY(p.X, p.Y, m.xSize, m.ySize, t)
                                    For i As Integer = b.minX To b.maxX Step 1
                                        For j As Integer = b.minY To b.maxY Step 1
                                            If m.board(i, j).locID.Count > 0 Then
                                                Dim locID As Integer = m.board(i, j).locID.Item(0) - 1
                                                If locID > -1 AndAlso Not usedLocs.Contains(locID) Then
                                                    For L As Integer = 0 To UBound(pl) Step 1
                                                        If Math.Abs(m.Loc(locID).pos.X - pl(L).X) < 2 _
                                                         And Math.Abs(m.Loc(locID).pos.Y - pl(L).Y) < 2 Then
                                                            possibleLocs.Add(locID)
                                                        End If
                                                    Next L
                                                End If
                                            End If
                                        Next j
                                    Next i
                                    If possibleLocs.Count = 0 Then
                                        If usedLocs.Count > 0 Then
                                            usedLocs.Clear()
                                        Else
                                            t += 1
                                        End If
                                    Else
                                        Exit Do
                                    End If
                                Loop
                                If possibleLocs.Count = 0 Then Throw New Exception("Не могу найти подходящую локацию")
                                Dim sel As Integer = comm.RandomSelection(possibleLocs, True)
                                m.board(p.X, p.Y).locID.Add(m.Loc(sel).ID)
                                usedLocs.Add(sel)
                            Next p
                        End If
                    Else
                        m.board(pp(0).X, pp(0).Y).locID.Add(pID(s)(selectedIDs(s)))
                    End If
                End If
            Loop

            Dim symmPoints(m.xSize, m.ySize)() As Point
            Dim tmpm As Map = m
            'Parallel.For(0, m.Loc.Length, Sub(i As Integer)
            For i As Integer = 0 To UBound(m.Loc) Step 1
                If Not tmpm.Loc(i).IsObtainedBySymmery Then
                    Dim n1(), n2, maxDiff, sel, myj As Integer
                    Dim t As Location.Borders
                    Dim b As New Location.Borders With {.minX = Integer.MaxValue, .maxX = Integer.MinValue, _
                                                        .miny = Integer.MaxValue, .maxy = Integer.MinValue}
                    For y As Integer = 0 To tmpm.ySize Step 1
                        For x As Integer = 0 To tmpm.xSize Step 1
                            If tmpm.board(x, y).locID.Item(0) = tmpm.Loc(i).ID Then
                                b.minX = Math.Min(b.minX, x)
                                b.minY = Math.Min(b.minY, y)
                                b.maxX = Math.Max(b.maxX, x)
                                b.maxY = Math.Max(b.maxY, y)
                                symmPoints(x, y) = symm.ApplySymm(New Point(x, y), settMap.nRaces, tmpm, 1)
                            End If
                        Next x
                    Next y

                    Dim tryagain As Boolean = True
                    Dim nIter As Integer = 0
                    Do While tryagain And nIter < 1000
                        tryagain = False
                        nIter += 1
                        For y As Integer = b.minY To b.maxY Step 1
                            For x As Integer = b.minX To b.maxX Step 1
                                'для каждой точки и ее отражений считаем соседей для нас myid<>neubourid для каждого id отдельно, для отражений myid=neubourid если выгоднее поменять местами - меняем. повторяем цикл, пока выгодно менять
                                If Not IsNothing(symmPoints(x, y)) AndAlso symmPoints(x, y).Length > 1 Then
                                    t = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                                    ReDim n1(UBound(tmpm.Loc))
                                    For q As Integer = t.minY To t.maxY Step 1
                                        For p As Integer = t.minX To t.maxX Step 1
                                            If Not tmpm.board(p, q).locID.Item(0) = tmpm.Loc(i).ID Then
                                                n1(tmpm.board(p, q).locID.Item(0) - 1) += 1
                                            End If
                                        Next p
                                    Next q
                                    maxDiff = 3
                                    sel = -1
                                    myj = -1
                                    For j As Integer = 0 To UBound(symmPoints(x, y)) Step 1
                                        If Not x = symmPoints(x, y)(j).X Or Not y = symmPoints(x, y)(j).Y Then
                                            n2 = 0
                                            t = NearestXY(symmPoints(x, y)(j).X, symmPoints(x, y)(j).Y, tmpm.xSize, tmpm.ySize, 1)
                                            For q As Integer = t.minY To t.maxY Step 1
                                                For p As Integer = t.minX To t.maxX Step 1
                                                    If tmpm.board(p, q).locID.Item(0) = tmpm.Loc(i).ID Then
                                                        n2 += 1
                                                    End If
                                                Next p
                                            Next q
                                            If n2 > 0 And n2 = n1(tmpm.board(symmPoints(x, y)(j).X, symmPoints(x, y)(j).Y).locID.Item(0) - 1) Then
                                                Dim d As Integer = n2
                                                If maxDiff < d Then
                                                    maxDiff = d
                                                    sel = j
                                                End If
                                            End If
                                        Else
                                            myj = j
                                        End If
                                    Next j
                                    If sel > -1 Then
                                        Dim tID1, tID2 As Integer
                                        tID1 = tmpm.board(symmPoints(x, y)(myj).X, symmPoints(x, y)(myj).Y).locID.Item(0)
                                        tID2 = tmpm.board(symmPoints(x, y)(sel).X, symmPoints(x, y)(sel).Y).locID.Item(0)
                                        tmpm.board(symmPoints(x, y)(myj).X, symmPoints(x, y)(myj).Y).locID.Clear()
                                        tmpm.board(symmPoints(x, y)(sel).X, symmPoints(x, y)(sel).Y).locID.Clear()
                                        tmpm.board(symmPoints(x, y)(myj).X, symmPoints(x, y)(myj).Y).locID.Add(tID2)
                                        tmpm.board(symmPoints(x, y)(sel).X, symmPoints(x, y)(sel).Y).locID.Add(tID1)
                                        b.minX = Math.Min(b.minX, symmPoints(x, y)(sel).X)
                                        b.minY = Math.Min(b.minY, symmPoints(x, y)(sel).Y)
                                        b.maxX = Math.Max(b.maxX, symmPoints(x, y)(sel).X)
                                        b.maxY = Math.Max(b.maxY, symmPoints(x, y)(sel).Y)
                                        If symmPoints(x, y).Length > 2 Then
                                            Dim tj1, tj2 As Integer
                                            tj1 = -1 : tj2 = -1
                                            For j As Integer = 0 To UBound(symmPoints(x, y)) Step 1
                                                If Not j = myj And Not j = sel Then
                                                    tj1 = j
                                                    Exit For
                                                End If
                                            Next j
                                            For j As Integer = tj1 + 1 To UBound(symmPoints(x, y)) Step 1
                                                If Not j = myj And Not j = sel Then
                                                    tj2 = j
                                                    Exit For
                                                End If
                                            Next j
                                            tID1 = tmpm.board(symmPoints(x, y)(tj1).X, symmPoints(x, y)(tj1).Y).locID.Item(0)
                                            tID2 = tmpm.board(symmPoints(x, y)(tj2).X, symmPoints(x, y)(tj2).Y).locID.Item(0)
                                            tmpm.board(symmPoints(x, y)(tj1).X, symmPoints(x, y)(tj1).Y).locID.Clear()
                                            tmpm.board(symmPoints(x, y)(tj2).X, symmPoints(x, y)(tj2).Y).locID.Clear()
                                            tmpm.board(symmPoints(x, y)(tj1).X, symmPoints(x, y)(tj1).Y).locID.Add(tID2)
                                            tmpm.board(symmPoints(x, y)(tj2).X, symmPoints(x, y)(tj2).Y).locID.Add(tID1)
                                        End If
                                        x = b.maxX
                                        y = b.maxY
                                        tryagain = True
                                    End If
                                End If
                            Next x
                        Next y
                    Loop
                End If
            Next i
            ' End Sub)
            m = tmpm
        Else
            Do While idlist.Count > 0
                Call makePointsList(m, idlist, allPoints, pID, Weight, minweight, selectedIDs, selectedWeight, calculatedWeights)
                If idlist.Count > 0 Then
                    Dim s As Integer = comm.RandomSelection(idlist, selectedWeight, True)
                    m.board(allPoints(s)(selectedIDs(s)).X, _
                            allPoints(s)(selectedIDs(s)).Y).locID.Add(pID(s)(selectedIDs(s)))
                End If
            Loop
        End If
    End Sub
    Private Sub makePointsList(ByRef m As Map, ByRef idlist As List(Of Integer), ByRef allPoints()() As Point, _
                               ByRef pID()() As Integer, ByRef Weight()() As Double, ByVal minweight As Double, _
                               ByRef selectedIDs() As Integer, ByRef selectedWeight() As Double, _
                               ByRef calculatedWeights(,,) As Double)
        Dim tmp_m As Map = m
        Dim tmp_allPoints()() As Point = allPoints
        Dim tmp_pID()() As Integer = pID
        Dim tmp_selectedIDs() As Integer = selectedIDs
        Dim tmp_Weight()() As Double = Weight
        Dim tmp_selectedWeight() As Double = selectedWeight
        Dim tmp_calculatedWeights(,,) As Double = calculatedWeights

        Parallel.For(0, tmp_m.ySize + 1, _
         Sub(y As Integer)
             Dim n As Integer = -1
             Dim u As Integer = UBound(tmp_allPoints(y))
             Dim b As Location.Borders
             For x As Integer = 0 To tmp_m.xSize Step 1
                 If tmp_m.board(x, y).locID.Count = 0 Then
                     b = NearestXY(x, y, tmp_m.xSize, tmp_m.ySize, 1)
                     For i As Integer = b.minX To b.maxX Step 1
                         For j As Integer = b.minY To b.maxY Step 1
                             If tmp_m.board(i, j).locID.Count > 0 Then
                                 n += 1
                                 If n > u Then
                                     u = 2 * u + 1
                                     ReDim Preserve tmp_allPoints(y)(u), tmp_pID(y)(u), tmp_Weight(y)(u)
                                 End If
                                 tmp_allPoints(y)(n) = New Point(x, y)
                                 tmp_pID(y)(n) = tmp_m.board(i, j).locID.Item(0)
                                 If tmp_calculatedWeights(x, y, tmp_pID(y)(n) - 1) > -1 Then
                                     tmp_Weight(y)(n) = tmp_calculatedWeights(x, y, tmp_pID(y)(n) - 1)
                                 Else
                                     tmp_Weight(y)(n) = Math.Max(tmp_m.Loc(tmp_pID(y)(n) - 1).pWeight(tmp_allPoints(y)(n)), minweight)
                                     tmp_calculatedWeights(x, y, tmp_pID(y)(n) - 1) = tmp_Weight(y)(n)
                                 End If
                             End If
                         Next j
                     Next i
                 End If
             Next x
             If n > -1 Then
                 tmp_selectedIDs(y) = rndgen.RndPos(n + 1, False) - 1
                 tmp_selectedWeight(y) = tmp_Weight(y)(tmp_selectedIDs(y))
             Else
                 tmp_selectedIDs(y) = -1
                 tmp_selectedWeight(y) = 0
             End If
         End Sub)
        idlist.Clear()
        For y As Integer = 0 To tmp_m.ySize Step 1
            If tmp_selectedIDs(y) > -1 Then
                idlist.Add(y)
            End If
        Next y
        m = tmp_m
        allPoints = tmp_allPoints
        pID = tmp_pID
        selectedIDs = tmp_selectedIDs
        Weight = tmp_Weight
        selectedWeight = tmp_selectedWeight
        calculatedWeights = tmp_calculatedWeights
    End Sub
    ''' <summary>Returns value from 0 To UBound(m.Loc)</summary>

    Private Sub SetBorders(ByRef m As Map, ByVal settMap As Map.SettingsMap, ByRef Term As TerminationCondition)

        Dim tmpm As Map = m
        Dim del(tmpm.xSize, tmpm.ySize), freeze(tmpm.xSize, tmpm.ySize) As Boolean
        Dim LocBorders(UBound(tmpm.Loc), UBound(tmpm.Loc)) As Dictionary(Of String, Point)

        Dim borderRadius(tmpm.xSize, tmpm.ySize) As Integer
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                borderRadius(x, y) = -1
            Next x
        Next y

        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                If borderRadius(x, y) = -1 Then
                    Dim b As Location.Borders = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                    Dim id As Integer = tmpm.board(x, y).locID.Item(0)
                    Dim isBorder As Boolean = False
                    For i As Integer = b.minX To b.maxX Step 1
                        For j As Integer = b.minY To b.maxY Step 1
                            If Not id = tmpm.board(i, j).locID.Item(0) Then
                                isBorder = True
                                i = b.maxX
                                Exit For
                            End If
                        Next j
                    Next i
                    If isBorder Then
                        borderRadius(x, y) = rndgen.RndPos(2, True) - 1
                        If tmpm.symmID > -1 Then
                            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, tmpm, 1)
                            For i As Integer = 0 To UBound(p) Step 1
                                borderRadius(p(i).X, p(i).Y) = borderRadius(x, y)
                            Next i
                        End If
                    End If
                End If
            Next x
        Next y
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                If borderRadius(x, y) > -1 Then
                    Dim id As Integer = tmpm.board(x, y).locID.Item(0)
                    freeze(x, y) = True
                    Dim b As Location.Borders = NearestXY(x, y, tmpm.xSize, tmpm.ySize, borderRadius(x, y))
                    For i As Integer = b.minX To b.maxX Step 1
                        For j As Integer = b.minY To b.maxY Step 1
                            tmpm.board(i, j).isBorder = True
                            If Not tmpm.board(i, j).locID.Contains(id) Then tmpm.board(i, j).locID.Add(id)
                        Next j
                    Next i
                End If
            Next x
        Next y
        borderRadius = Nothing
        Dim nNeighbours(tmpm.xSize, tmpm.ySize) As Integer

        For repeatloop As Integer = 0 To 2 Step 1
            Parallel.For(0, tmpm.ySize + 1, _
             Sub(y As Integer)
                 Dim n As Integer
                 Dim b As Location.Borders
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If Not freeze(x, y) Then
                         n = 0
                         b = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                         For i As Integer = b.minX To b.maxX Step 1
                             For j As Integer = b.minY To b.maxY Step 1
                                 If Not tmpm.board(i, j).isBorder Then
                                     n += 1
                                 End If
                             Next j
                         Next i
                         nNeighbours(x, y) = n
                     End If
                 Next x
             End Sub)
            For y As Integer = 0 To tmpm.ySize Step 1
                For x As Integer = 0 To tmpm.xSize Step 1
                    If nNeighbours(x, y) > 1 Then
                        Dim r As Integer = rndgen.RndPos(5, False)
                        If Math.Abs(nNeighbours(x, y) - 5) > r Then del(x, y) = True
                        nNeighbours(x, y) = 0
                        If tmpm.symmID > -1 Then
                            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, tmpm, 1)
                            For i As Integer = 0 To UBound(p) Step 1
                                nNeighbours(p(i).X, p(i).Y) = nNeighbours(x, y)
                                del(p(i).X, p(i).Y) = del(x, y)
                            Next i
                        End If
                    ElseIf nNeighbours(x, y) = 1 Then
                        nNeighbours(x, y) = 0
                    End If
                Next x
            Next y
            Parallel.For(0, tmpm.ySize + 1, _
             Sub(y As Integer)
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If del(x, y) Then
                         tmpm.board(x, y).isBorder = False
                         del(x, y) = False
                     End If
                 Next x
             End Sub)
        Next repeatloop
        freeze = Nothing
        nNeighbours = Nothing

        For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
            For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                LocBorders(i, j) = New Dictionary(Of String, Point)
            Next j
        Next i
        For y As Integer = 0 To tmpm.ySize Step 1
            For x As Integer = 0 To tmpm.xSize Step 1
                If tmpm.board(x, y).isBorder Then
                    Dim id As Integer = tmpm.board(x, y).locID.Item(0)
                    Dim b As Location.Borders = NearestXY(x, y, tmpm.xSize, tmpm.ySize, 1)
                    For i As Integer = b.minX To b.maxX Step 1
                        For j As Integer = b.minY To b.maxY Step 1
                            Dim n As Integer = tmpm.board(i, j).locID.Item(0)
                            If Not id = n Then
                                Dim s As String = i & "_" & j
                                Dim LID1 As Integer = Math.Min(id, n) - 1
                                Dim LID2 As Integer = Math.Max(id, n) - 1
                                If Not LocBorders(LID1, LID2).ContainsKey(s) Then LocBorders(LID1, LID2).Add(s, New Point(i, j))
                            End If
                        Next j
                    Next i
                End If
            Next x
        Next y
        For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
            For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                If LocBorders(i, j).Count < settMap.minPassWidth + 0.5 * settMap.minPassDist Then LocBorders(i, j).Clear()
            Next j
        Next i
        Dim delList As New List(Of String)
        Dim nearRLocs As New List(Of Integer)
        For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
            For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                Dim dR As Double
                If i < settMap.nRaces Then
                    dR = 2 * (settMap.minPassDist + 1)
                Else
                    dR = 0.7 * settMap.minPassDist + 1
                End If
                Do While dR >= 1
                    delList.Clear()
                    For Each k As String In LocBorders(i, j).Keys
                        Dim p As Point = LocBorders(i, j).Item(k)
                        Dim minD As Double = 2 * settMap.minPassWidth + 1 + dR
                        Dim b As Location.Borders = NearestXY(p.X, p.Y, tmpm.xSize, tmpm.ySize, CInt(Math.Ceiling(minD)))
                        minD *= minD
                        nearRLocs.Clear()
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                Dim id As Integer = tmpm.board(x, y).locID.Item(0) - 1
                                If (i >= settMap.nRaces And Not id = i And Not id = j AndAlso Not nearRLocs.Contains(id)) _
                                OrElse (i < settMap.nRaces And id < settMap.nRaces AndAlso Not nearRLocs.Contains(id)) Then
                                    If CDbl(p.SqDist(x, y)) <= minD Then
                                        nearRLocs.Add(id)
                                        If nearRLocs.Count > 1 Then
                                            x = b.maxX
                                            y = b.maxY
                                        End If
                                    End If
                                End If
                            Next y
                        Next x
                        If nearRLocs.Count > 1 Then delList.Add(k)
                    Next k
                    If delList.Count > 0 Then
                        If delList.Count = LocBorders(i, j).Count Then
                            dR *= 0.9
                        Else
                            dR = 0
                        End If
                    Else
                        dR = 0
                    End If
                Loop
                If delList.Count > 0 Then
                    If delList.Count = LocBorders(i, j).Count Then
                        Dim xsum, ysum, minD, D As Integer
                        Dim k As String = ""
                        xsum = 0
                        ysum = 0
                        minD = Integer.MaxValue
                        For Each p As Point In LocBorders(i, j).Values
                            xsum += p.X
                            ysum += p.Y
                        Next p
                        xsum = CInt(xsum / LocBorders(i, j).Count)
                        ysum = CInt(ysum / LocBorders(i, j).Count)
                        For Each p As Point In LocBorders(i, j).Values
                            D = p.SqDist(xsum, ysum)
                            If D < minD Or (D = minD AndAlso rndgen.PRand(0, 1) > 0.5) Then
                                minD = D
                                k = p.X & "_" & p.Y
                            End If
                        Next p
                        delList.Remove(k)
                    End If
                    For Each k As String In delList
                        LocBorders(i, j).Remove(k)
                    Next k
                End If
            Next j
        Next i

        Dim equalLocPairsList As New List(Of String)
        If tmpm.symmID > -1 Then
            Dim Id(1)(), a() As Integer
            Dim freezed As New List(Of String)
            For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
                For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                    If LocBorders(i, j).Count > 0 Then
                        freezed.Add(i & "_" & j)
                        a = New Integer() {i, j}
                        Parallel.For(0, 2, _
                         Sub(k As Integer)
                             Dim p() As Point = symm.ApplySymm(tmpm.Loc(a(k)).pos, settMap.nRaces, tmpm, 1)
                             ReDim Id(k)(UBound(p))
                             For q As Integer = 0 To UBound(p) Step 1
                                 Id(k)(q) = Location.FindLocIDByPosition(tmpm, p(q))
                             Next q
                         End Sub)
                        For r As Integer = 0 To UBound(Id(0)) Step 1
                            For t As Integer = 0 To UBound(Id(1)) Step 1
                                If Not Id(0)(r) = Id(1)(t) Then
                                    Dim LID1 As Integer = Math.Min(Id(0)(r), Id(1)(t))
                                    Dim LID2 As Integer = Math.Max(Id(0)(r), Id(1)(t))
                                    Dim s As String = LID1 & "_" & LID2
                                    If Not equalLocPairsList.Contains(s) AndAlso Not freezed.Contains(s) Then equalLocPairsList.Add(s)
                                End If
                            Next t
                        Next r
                    End If
                Next j
            Next i
        End If

        Parallel.For(0, UBound(tmpm.Loc), _
         Sub(i As Integer)
             Dim ids As New List(Of Integer)
             Dim selected, delete As New List(Of Integer)
             Dim startI As Integer = i + 1
             If i < settMap.nRaces Then
                 For j As Integer = settMap.nRaces To UBound(tmpm.Loc) Step 1
                     If LocBorders(i, j).Count > 0 Then
                         startI = settMap.nRaces
                         Exit For
                     End If
                 Next j
             End If
             For j As Integer = startI To UBound(tmpm.Loc) Step 1
                 If LocBorders(i, j).Count > 0 AndAlso Not equalLocPairsList.Contains(i & "_" & j) Then
                     Dim maxD, D As Integer
                     Dim pointsslist(LocBorders(i, j).Count - 1) As Point
                     LocBorders(i, j).Values.CopyTo(pointsslist, 0)
                     maxD = 0
                     ids.Clear()
                     selected.Clear()
                     delete.Clear()
                     For p1 As Integer = 0 To UBound(pointsslist) - 1 Step 1
                         ids.Add(p1)
                         For p2 As Integer = p1 + 1 To UBound(pointsslist) Step 1
                             D = pointsslist(p1).SqDist(pointsslist(p2))
                             If maxD < D Then maxD = D
                         Next p2
                     Next p1
                     ids.Add(UBound(pointsslist))
                     Dim maxPaths As Integer = CInt(Math.Max(Math.Sqrt(maxD) / settMap.minPassDist, 1))
                     For k As Integer = 1 To maxPaths Step 1
                         Dim s As Integer = comm.RandomSelection(ids, False)
                         selected.Add(s)
                         ids.Remove(s)
                         For Each p As Integer In ids
                             D = pointsslist(s).SqDist(pointsslist(p))
                             If D < settMap.minPassDist * settMap.minPassDist Then delete.Add(p)
                         Next p
                         For Each p As Integer In delete
                             ids.Remove(p)
                         Next p
                         If ids.Count = 0 Then Exit For
                     Next k
                     Dim Centers() As Point = New Point() {tmpm.Loc(i).pos, tmpm.Loc(j).pos}
                     For Each c As Point In Centers
                         For Each p As Integer In selected
                             Call MakePass(tmpm, pointsslist(p), c, settMap)
                         Next p
                     Next c
                 End If
             Next j
         End Sub)

        Term = New TerminationCondition(Term.maxTime)
        Call ConnectDisconnectedAreas(tmpm, settMap, Term)
        If Term.ExitFromLoops Then Exit Sub
        Parallel.For(0, tmpm.ySize + 1, _
         Sub(y As Integer)
             For x As Integer = 0 To tmpm.xSize Step 1
                 If Not tmpm.board(x, y).isBorder And tmpm.board(x, y).locID.Count > 1 Then
                     Dim n As Integer = tmpm.board(x, y).locID.Item(0)
                     tmpm.board(x, y).locID.Clear()
                     tmpm.board(x, y).locID.Add(n)
                 End If
             Next x
         End Sub)
        m = tmpm
    End Sub
    Private Sub ConnectDisconnectedAreas(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                         ByRef Term As TerminationCondition)
        Dim conn2(0, 0) As Boolean
        Do While Not IsNothing(conn2)
            Term.CheckTime()
            If Term.ExitFromLoops Then Exit Sub
            conn2 = Nothing
            Dim init As Point = Nothing
            For x As Integer = 0 To m.xSize Step 1
                For y As Integer = 0 To m.ySize Step 1
                    If Not m.board(x, y).isBorder Then
                        init = New Point(x, y)
                        Exit For
                    End If
                Next y
                If Not IsNothing(init) Then Exit For
            Next x
            Dim conn1(,) As Boolean = FindConnected(m, init)
            Dim dp As Point = FindDisconnected(m, conn1)
            If Not IsNothing(dp) Then
                conn2 = FindConnected(m, dp)
                Dim minD As Integer = Integer.MaxValue
                Dim D As Integer
                Dim p1, p2 As Point
                p1 = Nothing : p2 = Nothing
                For y1 As Integer = 0 To m.ySize Step 1
                    For x1 As Integer = 0 To m.xSize Step 1
                        If conn1(x1, y1) Then
                            For y2 As Integer = 0 To m.ySize Step 1
                                For x2 As Integer = 0 To m.xSize Step 1
                                    If conn2(x2, y2) Then
                                        D = New Point(x1, y1).SqDist(x2, y2)
                                        If minD > D Then
                                            minD = D
                                            p1 = New Point(x1, y1)
                                            p2 = New Point(x2, y2)
                                        End If
                                    End If
                                Next x2
                            Next y2
                        End If
                    Next x1
                Next y1
                Call MakePass(m, p1, p2, settMap)
            End If
        Loop
    End Sub
    Private Sub MakePass(ByRef m As Map, ByRef init As Point, ByRef dest As Point, _
                         ByRef settMap As Map.SettingsMap)
        Dim vx As Double = dest.X - init.X
        Dim vy As Double = dest.Y - init.Y
        Dim n As Integer = CInt(10 * Math.Max((vx * vx + vy * vy), 1))
        vx /= n
        vy /= n
        n += 10
        Dim tx, ty As Double
        Dim b As Location.Borders
        For r As Integer = 0 To n Step 1
            tx = init.X + CDbl(r) * vx
            ty = init.Y + CDbl(r) * vy
            b = NearestXY(CInt(tx), CInt(ty), m.xSize, m.ySize, CInt(2 * (0.5 * settMap.minPassWidth + 1)))
            For x As Integer = b.minX To b.maxX Step 1
                For y As Integer = b.minY To b.maxY Step 1
                    If m.board(x, y).isBorder Then
                        Dim dist As Double = Math.Sqrt(CDbl(x - tx) ^ 2 + CDbl(y - ty) ^ 2)
                        If dist < 0.5 * settMap.minPassWidth OrElse (dist <= settMap.minPassWidth AndAlso rndgen.PRand(0, 1) > 0.5) Then
                            Dim c1 As Integer = Math.Max(Math.Min(x, m.xSize - 1), 1)
                            Dim c2 As Integer = Math.Max(Math.Min(y, m.ySize - 1), 1)
                            Dim p() As Point
                            If m.symmID > -1 Then
                                p = symm.ApplySymm(New Point(c1, c2), settMap.nRaces, m, 1)
                            Else
                                p = New Point() {New Point(c1, c2)}
                            End If
                            For Each item As Point In p
                                If m.board(item.X, item.Y).isBorder Then
                                    m.board(item.X, item.Y).isBorder = False
                                    m.board(item.X, item.Y).isPass = True
                                ElseIf m.board(item.X, item.Y).isAttended Then
                                    Dim tb As Location.Borders = NearestXY(item, m, 1)
                                    For xx As Integer = tb.minX To tb.maxX Step 1
                                        For yy As Integer = tb.minY To tb.maxY Step 1
                                            If m.board(xx, yy).isBorder Then
                                                m.board(xx, yy).isBorder = False
                                                m.board(xx, yy).isPass = True
                                            End If
                                        Next yy
                                    Next xx
                                End If
                            Next item
                        End If
                    End If
                Next y
            Next x
        Next r
    End Sub
    Friend Function FindConnected(ByRef m As Map, ByRef init As Point) As Boolean(,)
        If m.board(init.X, init.Y).isBorder Then Throw New Exception("Find connected function: начальная точка непроходима")
        Dim connected(m.xSize, m.ySize), check(m.xSize, m.ySize) As Boolean
        check(init.X, init.Y) = True
        connected(init.X, init.Y) = True
        Dim r As Integer = 1
        Do While r > 0
            For j As Integer = 0 To m.ySize Step 1
                For i As Integer = 0 To m.xSize Step 1
                    If check(i, j) Then
                        Dim b As Location.Borders = NearestXY(i, j, m.xSize, m.ySize, 1)
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                If Not m.board(x, y).isBorder And Not connected(x, y) Then
                                    connected(x, y) = True
                                    check(x, y) = True
                                    r += 1
                                End If
                            Next y
                        Next x
                        check(i, j) = False
                        r -= 1
                    End If
                Next i
            Next j
        Loop
        Return connected
    End Function
    Friend Function FindConnected(ByRef free(,) As Boolean, ByRef init As Point) As Boolean(,)
        Dim m As Map = FreeToMap(free)
        Return FindConnected(m, init)
    End Function
    Private Function FreeToMap(ByRef free(,) As Boolean) As Map
        Dim m As New Map(UBound(free, 1), UBound(free, 2), -1)
        For j As Integer = 0 To m.ySize Step 1
            For i As Integer = 0 To m.xSize Step 1
                m.board(i, j).isBorder = Not free(i, j)
            Next i
        Next j
        Return m
    End Function
    Private Function FindDisconnected(ByRef m As Map, ByRef connected(,) As Boolean) As Point
        If Not IsNothing(connected) AndAlso connected.Length > 0 Then
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If Not m.board(x, y).isBorder And Not connected(x, y) Then Return New Point(x, y)
                Next x
            Next y
        Else
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If Not m.board(x, y).isBorder Then Return New Point(x, y)
                Next x
            Next y
        End If
        Return Nothing
    End Function
    Friend Function FindDisconnected(ByRef free(,) As Boolean, ByRef connected(,) As Boolean) As Point
        Dim m As Map = FreeToMap(free)
        Return FindDisconnected(m, connected)
    End Function
    Friend Function FindDisconnected(ByRef free(,) As Boolean, ByRef connected()(,) As Boolean) As Point
        If IsNothing(connected) Then Return FindDisconnected(free, New Boolean(,) {})
        Dim c(UBound(connected(0), 1), UBound(connected(0), 2)) As Boolean
        For i As Integer = 0 To UBound(connected) Step 1
            For y As Integer = 0 To UBound(c, 2) Step 1
                For x As Integer = 0 To UBound(c, 1) Step 1
                    c(x, y) = c(x, y) Or connected(i)(x, y)
                Next x
            Next y
        Next i
        Return FindDisconnected(free, c)
    End Function

    Private Sub PlaceActiveObjects(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                                   ByRef settRaceLoc As Map.SettingsLoc, ByVal settCommLoc As Map.SettingsLoc, _
                                   ByRef ObjectBlank()(,) As Map.Cell, ByRef Term As TerminationCondition)
        Dim tmpm As Map = m
        Dim LocsPlacing(UBound(tmpm.Loc)) As Location.Borders
        Dim LocArea(UBound(tmpm.Loc))() As Integer
        Dim LocSymmMult(UBound(tmpm.Loc)) As Double
        Dim LocFreeCells(UBound(tmpm.Loc))(,) As Boolean

        Parallel.For(0, tmpm.Loc.Length, _
         Sub(Li As Integer)
             Dim id As Integer = tmpm.Loc(Li).ID
             LocsPlacing(Li) = New Location.Borders With {.minX = Integer.MaxValue, .minY = Integer.MaxValue, _
                                                          .maxX = Integer.MinValue, .maxY = Integer.MinValue}
             LocArea(id - 1) = New Integer() {0, 0}
             For y As Integer = 0 To tmpm.ySize Step 1
                 For x As Integer = 0 To tmpm.xSize Step 1
                     If tmpm.board(x, y).locID.Item(0) = id Then
                         LocsPlacing(Li).minX = Math.Min(LocsPlacing(Li).minX, x)
                         LocsPlacing(Li).minY = Math.Min(LocsPlacing(Li).minY, y)
                         LocsPlacing(Li).maxX = Math.Max(LocsPlacing(Li).maxX, x)
                         LocsPlacing(Li).maxY = Math.Max(LocsPlacing(Li).maxY, y)
                     End If
                 Next x
             Next y
             Dim dx As Integer = LocsPlacing(id - 1).minX
             Dim dy As Integer = LocsPlacing(id - 1).minY
             Dim freeCells(LocsPlacing(id - 1).maxX - dx, _
                           LocsPlacing(id - 1).maxY - dy) As Boolean
             For y As Integer = LocsPlacing(id - 1).minY To LocsPlacing(id - 1).maxY Step 1
                 For x As Integer = LocsPlacing(id - 1).minX To LocsPlacing(id - 1).maxX Step 1
                     If Not tmpm.board(x, y).isBorder _
                      And Not tmpm.board(x, y).isAttended _
                      AndAlso tmpm.board(x, y).locID.Item(0) = id Then
                         LocArea(id - 1)(0) += 1
                         freeCells(x - dx, y - dy) = True
                     End If
                 Next x
             Next y

             Dim fc(,) As Boolean = CType(freeCells.Clone, Boolean(,))
             If tmpm.symmID > -1 Then
                 Dim p() As Point
                 For y As Integer = 0 To UBound(freeCells, 2) Step 1
                     For x As Integer = 0 To UBound(freeCells, 1) Step 1
                         If freeCells(x, y) Then
                             p = symm.ApplySymm(New Point(x + dx, y + dy), settMap.nRaces, tmpm, 1)
                             For i As Integer = 0 To UBound(p) Step 1
                                 Dim tx As Integer = p(i).X - dx
                                 Dim ty As Integer = p(i).Y - dy
                                 If Not tx = x Or Not ty = y Then
                                     If tx > -1 And ty > -1 And tx <= UBound(freeCells, 1) And ty <= UBound(freeCells, 2) Then

                                         Dim b As Location.Borders = NearestXY(tx, ty, UBound(freeCells, 1), UBound(freeCells, 2), 1)
                                         For q As Integer = b.minY To b.maxY Step 1
                                             For w As Integer = b.minX To b.maxX Step 1
                                                 freeCells(w, q) = False
                                             Next w
                                         Next q
                                     End If
                                 End If
                             Next i
                         End If
                     Next x
                 Next y
             End If
             Dim PlaceCells(UBound(freeCells, 1), UBound(freeCells, 2)) As Boolean
             For y As Integer = 0 To UBound(freeCells, 2) Step 1
                 For x As Integer = 0 To UBound(freeCells, 1) Step 1
                     If freeCells(x, y) AndAlso MayPlaceObject(freeCells, 8, x, y) Then
                         Dim b As Location.Borders = NearestXY(x, y, UBound(freeCells, 1), UBound(freeCells, 2), 1)
                         For q As Integer = b.minY To b.maxY Step 1
                             For p As Integer = b.minX To b.maxX Step 1
                                 PlaceCells(p, q) = True
                             Next p
                         Next q
                     End If
                 Next x
             Next y

             For y As Integer = 0 To UBound(freeCells, 2) Step 1
                 For x As Integer = 0 To UBound(freeCells, 1) Step 1
                     If PlaceCells(x, y) Then LocArea(id - 1)(1) += 1
                 Next x
             Next y
             If LocArea(id - 1)(0) > 0 And LocArea(id - 1)(1) = 0 Then
                 Dim ids As New List(Of Integer)
                 Dim p(freeCells.Length - 1) As Point
                 Dim n As Integer = -1
                 For y As Integer = 1 To UBound(freeCells, 2) - 1 Step 1
                     For x As Integer = 1 To UBound(freeCells, 1) - 1 Step 1
                         If freeCells(x, y) Then
                             n += 1
                             p(n) = New Point(x, y)
                             ids.Add(n)
                         End If
                     Next x
                 Next y
                 If ids.Count > 0 Then
                     Dim s As Integer = comm.RandomSelection(ids, True)
                     Dim x As Integer = p(s).X
                     Dim y As Integer = p(s).Y
                     For qq As Integer = y - 1 To y + 1 Step 1
                         For pp As Integer = x - 1 To x + 1 Step 1
                             freeCells(pp, qq) = True
                             LocArea(id - 1)(1) += 1
                         Next pp
                     Next qq
                 End If
             End If
             LocFreeCells(id - 1) = freeCells

             If tmpm.symmID > -1 Then
                 Dim n As Integer = 0
                 Dim pp() As Point = New Point() {New Point(LocsPlacing(Li).minX, LocsPlacing(Li).minY), _
                                                  New Point(LocsPlacing(Li).minX, LocsPlacing(Li).maxY), _
                                                  New Point(LocsPlacing(Li).maxX, LocsPlacing(Li).minY), _
                                                  New Point(LocsPlacing(Li).maxX, LocsPlacing(Li).maxY)}
                 Dim p() As Point = symm.ApplySymm(pp(0), settMap.nRaces, tmpm, 1)
                 For q As Integer = 0 To UBound(p) Step 1
                     For w As Integer = 0 To UBound(pp) Step 1
                         If p(q).X = pp(w).X And p(q).Y = pp(w).Y Then
                             n += 1
                             Exit For
                         End If
                     Next w
                 Next q
                 LocSymmMult(Li) = 1 / n
             Else
                 LocSymmMult(Li) = 1
             End If
         End Sub)

        Dim GroupID As Integer = 1
        Term = New TerminationCondition(Term.maxTime)
        Call FillLocation(GroupID, 1, tmpm, LocsPlacing, LocArea, settMap, settRaceLoc, _
                          tmpm.symmID, True, LocSymmMult, LocFreeCells, Term)
        If Term.ExitFromLoops Then Exit Sub
        Dim TT(UBound(tmpm.Loc)) As TerminationCondition
        Dim maxTime As Long = Term.maxTime

        'Parallel.For(settMap.nRaces, tmpm.Loc.Length, _
        ' Sub(i As Integer)
        For i As Integer = settMap.nRaces To UBound(tmpm.Loc) Step 1
            If Not tmpm.Loc(i).IsObtainedBySymmery Then
                TT(i) = New TerminationCondition(maxTime)
                For j As Integer = 0 To UBound(tmpm.Loc) Step 1
                    If Not IsNothing(TT(j)) Then TT(i).ExitFromLoops = TT(i).ExitFromLoops Or TT(j).ExitFromLoops
                Next j
                If TT(i).ExitFromLoops Then Exit Sub
                Call FillLocation(GroupID, tmpm.Loc(i).ID, tmpm, LocsPlacing, LocArea, settMap, settCommLoc, _
                                  tmpm.symmID, False, LocSymmMult, LocFreeCells, TT(i))
            End If
        Next i
        ' End Sub)
        For i As Integer = 0 To UBound(tmpm.Loc) Step 1
            If Not IsNothing(TT(i)) Then Term.ExitFromLoops = Term.ExitFromLoops Or TT(i).ExitFromLoops
        Next i
        If Term.ExitFromLoops Then Exit Sub

        Dim maxGroupID As Integer = ImpenetrableMeshGen.GetMaxGroupID(tmpm)

        For y As Integer = tmpm.ySize To 0 Step -1
            For x As Integer = tmpm.xSize To 0 Step -1
                If tmpm.board(x, y).objectID > 0 Then
                    Dim id As Integer = tmpm.board(x, y).objectID
                    Dim gid As Integer = tmpm.board(x, y).groupID
                    Dim d As Integer = ActiveObjects(id).dxy
                    For j As Integer = 0 To UBound(ObjectBlank(id), 2) Step 1
                        Dim dy As Integer = y + j
                        For i As Integer = 0 To UBound(ObjectBlank(id), 1) Step 1
                            Dim dx As Integer = x + i
                            If dx <= tmpm.xSize And dy <= tmpm.ySize Then
                                tmpm.board(dx, dy).groupID = ObjectBlank(id)(i, j).groupID
                                tmpm.board(dx, dy).GuardLoc = ObjectBlank(id)(i, j).GuardLoc
                                If tmpm.board(dx, dy).GuardLoc Then tmpm.board(dx, dy).isObjectGuard = True
                                tmpm.board(dx, dy).isAttended = ObjectBlank(id)(i, j).isAttended
                                tmpm.board(dx, dy).isBorder = ObjectBlank(id)(i, j).isBorder
                                tmpm.board(dx, dy).Penetrable = ObjectBlank(id)(i, j).Penetrable
                                tmpm.board(dx, dy).objectID = ObjectBlank(id)(i, j).objectID
                                If ObjectBlank(id)(i, j).isAttended Or ObjectBlank(id)(i, j).isBorder Then
                                    tmpm.board(dx, dy).isPass = False
                                End If
                            ElseIf ObjectBlank(id)(i, j).GuardLoc Or ObjectBlank(id)(i, j).isAttended Or ObjectBlank(id)(i, j).isBorder Or ObjectBlank(id)(i, j).objectID > 0 Then
                                Throw New Exception("Неправильно поставлен объект: objectID = " & id & " position = " & x & "; " & y)
                            End If
                        Next i
                    Next j
                    tmpm.board(x + d, y + d).groupID = gid
                    If ActiveObjects(id).hasExternalGuard Then
                        maxGroupID += 1
                        tmpm.board(x + d + ActiveObjects(id).Size, _
                                   y + d + ActiveObjects(id).Size).groupID = maxGroupID 'gid
                    End If
                End If
            Next x
        Next y

    End Sub
    Private Sub ObjectsPlacingVariants(ByRef objIDs() As Integer, ByRef locID As Integer, _
                                       ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                       ByRef LocsPlacing() As Location.Borders, _
                                       ByRef FreeCells(,) As Boolean, _
                                       ByRef NearWith() As Integer, ByRef symmID As Integer, _
                                       ByRef output() As Point, ByRef Term As TerminationCondition)
        If IsNothing(NearWith) Then
            ReDim NearWith(UBound(objIDs))
            For i As Integer = 0 To UBound(NearWith) Step 1
                NearWith(i) = -1
            Next i
        End If
        Dim dx As Integer = LocsPlacing(locID - 1).minX
        Dim dy As Integer = LocsPlacing(locID - 1).minY

        Dim tmpIDs() As Integer = CType(objIDs.Clone, Integer())

        Call Term.CheckTime()
        If Term.ExitFromLoops Then Exit Sub

        Dim bestOutput(UBound(tmpIDs)) As Point
        Dim maxN As Integer = 0
        ReDim output(UBound(tmpIDs))
        Call PlaceObjRow(tmpIDs, 0, NearWith, _
                         New Point(m.Loc(locID - 1).pos.X - dx, m.Loc(locID - 1).pos.Y - dy), _
                         FreeCells, output, Term, maxN, bestOutput)
        If maxN = 0 And IsNothing(output(0)) Then
            output = Nothing
            Exit Sub
        ElseIf IsNothing(output(0)) Then
            ReDim Preserve bestOutput(maxN - 1)
            output = bestOutput
        End If
        For i As Integer = 0 To UBound(output) Step 1
            If Not IsNothing(output(i)) Then
                output(i) = New Point(output(i).X + dx, output(i).Y + dy)
            Else
                ReDim Preserve output(i - 1)
                Exit For
            End If
        Next i
    End Sub
    Private Sub PlaceObjRow(ByRef objIDs() As Integer, ByRef n As Integer, _
                            ByRef NearWith() As Integer, ByRef LocCenter As Point, _
                            ByRef freeCells(,) As Boolean, ByRef output() As Point, _
                            ByRef Term As TerminationCondition,
                            ByRef maxN As Integer, ByRef bestOutput() As Point)
        If Term.ExitFromLoops Then Exit Sub

        If n > maxN Then
            For i As Integer = 0 To n - 1 Step 1
                bestOutput(i) = New Point(output(i).X, output(i).Y)
            Next i
            maxN = n
        End If

        Dim fc_bak(,) As Boolean = CType(freeCells.Clone, Boolean(,))
        Dim fc(,) As Boolean
        Dim selected As Integer
        'For i As Integer = 0 To n - 1 Step 1
        '    Call PlaceObject(fc_bak, objIDs(i), output(i).X, output(i).Y)
        'Next i
        Dim pointsList(freeCells.Length - 1) As Point
        Dim Weight(UBound(pointsList)) As Double
        Dim pID As New List(Of Integer)
        Dim np As Integer = -1
        Dim desR, R, sigma As Double
        If NearWith(n) > -1 Then
            desR = (ActiveObjects(n).Size + ActiveObjects(objIDs(NearWith(n))).Size + 5) * 0.25 * (1 + Math.Sqrt(2))
            sigma = 0.1
        ElseIf objIDs(n) = 1 Then
            desR = 2
            sigma = 2
        End If
        For y As Integer = 0 To UBound(freeCells, 2) Step 1
            For x As Integer = 0 To UBound(freeCells, 1) Step 1
                If MayPlaceObject(fc_bak, objIDs(n), x, y) Then
                    np += 1
                    pointsList(np) = New Point(x, y)
                    pID.Add(np)
                    If NearWith(n) > -1 Then
                        R = output(NearWith(n)).Dist(pointsList(np))
                        Weight(np) = comm.Gauss(R, desR, sigma)
                    ElseIf objIDs(n) = 1 Then
                        R = LocCenter.Dist(pointsList(np))
                        Weight(np) = comm.Gauss(R, desR, sigma)
                    Else
                        Weight(np) = 1
                        For i As Integer = 0 To n - 1 Step 1
                            R = output(i).Dist(pointsList(np))
                            Weight(np) *= (10 + R)
                        Next i
                    End If
                    Weight(np) = Math.Max(Weight(np), 0.000001)
                End If
            Next x
        Next y
        If np = -1 Then
            output(n) = Nothing
            Exit Sub
        End If
        ReDim Preserve pointsList(np), Weight(np)
        Dim checkN As Integer = Math.Min(10, pID.Count)
        If n < UBound(objIDs) Then
            Do While pID.Count > 0
                fc = CType(fc_bak.Clone, Boolean(,))
                selected = comm.RandomSelection(pID, False)
                pID.Remove(selected)
                output(n) = New Point(pointsList(selected).X, pointsList(selected).Y)
                Call PlaceObject(fc, objIDs(n), output(n).X, output(n).Y)
                Call PlaceObjRow(objIDs, n + 1, NearWith, LocCenter, fc, output, Term, maxN, bestOutput)
                If Term.ExitFromLoops Then Exit Sub
                If Not IsNothing(output(n + 1)) Then
                    pID.Clear()
                Else
                    checkN -= 1
                    If checkN = 0 Then
                        Call Term.CheckTime()
                        If Term.ExitFromLoops Then Exit Sub
                        checkN = 10
                    End If
                End If
            Loop
            If IsNothing(output(n + 1)) Then output(n) = Nothing
        Else
            selected = comm.RandomSelection(pID, False)
            pID.Clear()
            output(n) = New Point(pointsList(selected).X, pointsList(selected).Y)
        End If
    End Sub
    Private Sub MakeLocObjectsList(ByRef places() As Integer, ByRef nearWith() As Integer, _
                                   ByRef sett As Map.SettingsLoc, ByRef isRaceLoc As Boolean, _
                                   ByRef LocArea() As Integer, ByRef symmID As Integer, _
                                   ByRef LocSymmMult As Double)
        Dim nCapital, nMinMines As Integer
        Dim mult As Double
        If isRaceLoc Then
            nCapital = 1
            mult = 1
            nMinMines = 3
        Else
            nCapital = 0
            mult = LocSymmMult * LocArea(0) / (Math.PI * (sett.AverageRadius - 2) ^ 2)
            If mult > 0.6 Then
                nMinMines = 2
            Else
                nMinMines = 1
            End If
        End If

        Dim areaUsed As Integer
        Dim DblnObj() As Double = New Double() {0, nCapital, _
                                                sett.maxCities, _
                                                sett.maxVendors, _
                                                sett.maxMercenaries, _
                                                sett.maxMages, _
                                                sett.maxTrainers, _
                                                sett.maxRuins, _
                                                sett.maxGoldMines, _
                                                sett.maxManaSources}
        Dim nObj(UBound(DblnObj) - 1) As Integer
        For i As Integer = 0 To UBound(DblnObj) Step 1
            Dim m As Double = mult * DblnObj(i)
            Dim n1 As Integer = CInt(Math.Floor(m))
            Dim n2 As Integer = CInt(Math.Ceiling(m))
            Dim r As Integer
            If n1 = n2 Then
                r = n1
            Else
                Dim d As Double = m - CDbl(n1)
                If rndgen.PRand(0, 1) > d Then
                    r = n1
                    If Not i = 8 And Not i = 9 Then DblnObj(8) += d
                Else
                    r = n2
                End If
            End If
            If i = 8 Or i = 9 Then
                nObj(8) += r
            Else
                nObj(i) = r
            End If
        Next i
        nObj(8) = Math.Max(nObj(8), 1)

        Dim sum As Integer = -1
        For i As Integer = 0 To UBound(nObj) Step 1
            sum += nObj(i)
        Next i
        ReDim places(sum), nearWith(sum)
        Dim p As Integer = 0
        Do While nObj(1) > 0
            nearWith(p) = -1
            Call AddObjId(places, nObj, 1, p, areaUsed)
            If areaUsed >= LocArea(1) Then
                ReDim Preserve places(p - 1), nearWith(p - 1)
                Exit Sub
            End If
        Loop
        Dim k As Integer = 0
        Dim placedMines As Integer = 0
        Do While nObj(8) > 0 And placedMines < nMinMines
            If isRaceLoc Then
                nearWith(p) = k
                k += 1
                If k = nCapital Then k = 0
            Else
                nearWith(p) = -1
            End If
            Call AddObjId(places, nObj, 8, p, areaUsed)
            If areaUsed >= LocArea(1) Then
                ReDim Preserve places(p - 1), nearWith(p - 1)
                Exit Sub
            End If
            placedMines += 1
        Loop
        Dim OWeight(UBound(ActiveObjects)) As Double
        For i As Integer = 1 To UBound(ActiveObjects) Step 1
            If ActiveObjects(i).hasExternalGuard Then
                OWeight(i) = 1 / (ActiveObjects(i).Size + 4) ^ 2
            Else
                OWeight(i) = 1 / (ActiveObjects(i).Size + 2) ^ 2
            End If
        Next i
        Dim AllObjList(sum - p), s As Integer
        Dim Weight(sum - p) As Double
        Dim ids As New List(Of Integer)
        Dim t As Integer = 0
        For i As Integer = 0 To UBound(nObj) Step 1
            For j As Integer = 1 To nObj(i) Step 1
                AllObjList(t) = i
                Weight(t) = OWeight(i)
                ids.Add(t)
                t += 1
            Next j
        Next i
        Do While ids.Count > 0
            s = comm.RandomSelection(ids, Weight, True)
            ids.Remove(s)
            nearWith(p) = -1
            Call AddObjId(places, nObj, AllObjList(s), p, areaUsed)
            If areaUsed >= LocArea(1) Then
                ReDim Preserve places(p - 1), nearWith(p - 1)
                Exit Sub
            End If
        Loop
    End Sub
    Private Sub AddObjId(ByRef places() As Integer, ByRef nObj() As Integer, ByRef id As Integer, _
                         ByRef p As Integer, ByRef AreaUsed As Integer)
        places(p) = ActiveObjects(id).TypeID
        AreaUsed = CInt(AreaUsed + (ActiveObjects(id).Area * 1.2))
        nObj(id) -= 1
        p += 1
    End Sub
    Private Sub FillLocation(ByRef GroupID As Integer, ByRef LocId As Integer, ByRef m As Map, ByRef LocsPlacing() As Location.Borders, _
                             ByRef LocArea()() As Integer, ByVal settMap As Map.SettingsMap, ByRef settLoc As Map.SettingsLoc, _
                             ByVal symmId As Integer, ByVal IsRaceLoc As Boolean, LocSymmMult() As Double, _
                             ByRef LocFreeCells()(,) As Boolean, ByRef Term As TerminationCondition)
        Dim tmpm As Map = m
        Dim tmpLocsPlacing() As Location.Borders = LocsPlacing
        Dim tmpLocFreeCells()(,) As Boolean = LocFreeCells
        Dim TT As TerminationCondition = Term
        Dim places(), nearWith() As Integer
        places = Nothing : nearWith = Nothing
        Call MakeLocObjectsList(places, nearWith, settLoc, IsRaceLoc, LocArea(LocId - 1), symmId, LocSymmMult(LocId - 1))
        Dim ok As Boolean = False
        Do While Not ok
            Call TT.CheckTime()
            If TT.ExitFromLoops Then
                Term = TT
                Exit Do
            End If
            Dim v()() As Point = Nothing
            If symmId > -1 Or Not IsRaceLoc Then
                ReDim v(0)
                Call ObjectsPlacingVariants(places, LocId, tmpm, settMap, tmpLocsPlacing, _
                                            tmpLocFreeCells(LocId - 1), nearWith, symmId, v(0), TT)
            Else
                ReDim v(settMap.nRaces - 1)
                Parallel.For(0, settMap.nRaces, _
                 Sub(i As Integer)
                     Call ObjectsPlacingVariants(places, i + 1, tmpm, settMap, tmpLocsPlacing, _
                                                 tmpLocFreeCells(i), nearWith, symmId, v(i), TT)
                 End Sub)
            End If
            If TT.ExitFromLoops Then
                Term = TT
                Exit Do
            End If
            nearWith = Nothing
            Dim minN As Integer = Integer.MaxValue
            For i As Integer = 0 To UBound(v) Step 1
                If IsNothing(v(i)) Then
                    If IsRaceLoc Then
                        Throw New Exception("Как минимум одна из стартовых локаций настолько маленькая, что я не могу разместить даже столицу")
                    Else
                        Exit Do
                    End If
                End If
                If minN > UBound(v(i)) Then minN = UBound(v(i))
            Next i
            For i As Integer = 0 To UBound(v) Step 1
                If minN < UBound(v(i)) Then ReDim Preserve v(i)(minN)
            Next i
            ok = True
            Dim minRsum, maxRsum, Rsum As Integer
            minRsum = Integer.MaxValue
            maxRsum = Integer.MinValue
            For i As Integer = 0 To UBound(v) Step 1
                Rsum = 0
                For n As Integer = 1 To minN Step 1
                    Rsum += v(i)(n).SqDist(v(i)(0))
                Next n
                minRsum = Math.Min(minRsum, Rsum)
                maxRsum = Math.Max(maxRsum, Rsum)
            Next i
            Dim tolerance As Double = 0.333 * (Math.Pow(1 + settLoc.maxRadiusDispersion, 2) + _
                         Math.Pow(1 + settLoc.maxEccentricityDispersion, 2) + 1.2 ^ 2)
            If minRsum * tolerance < maxRsum Then ok = False
            If ok Then
                Dim tmp_G As Integer = GroupID
                Parallel.For(0, v.Length, _
                 Sub(i As Integer)
                     Dim g As Integer = tmp_G + i * (minN + 1)
                     For n As Integer = 0 To minN Step 1
                         Call PlaceObject(tmpm, places(n), v(i)(n).X, v(i)(n).Y, g + n, settMap)
                     Next n
                 End Sub)
                GroupID += v.Length * (minN + 1)
            End If
        Loop
        m = tmpm
    End Sub
    Friend Shared Function GetMaxGroupID(ByRef m As Map) As Integer
        Dim maxGroupID As Integer = -1
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                maxGroupID = Math.Max(maxGroupID, m.board(x, y).groupID)
            Next x
        Next y
        Return maxGroupID
    End Function

    Friend Function ObjectBorders(ByRef id As Integer, ByRef x As Integer, ByRef y As Integer) As Location.Borders
        Dim res As Location.Borders
        res.minX = x - 1
        res.minY = y - 1
        res.maxX = x + ActiveObjects(id).Size
        res.maxY = y + ActiveObjects(id).Size
        If ActiveObjects(id).hasExternalGuard Then
            res.minX -= 1
            res.minY -= 1
            res.maxX += 1
            res.maxY += 1
        End If
        Return res
    End Function
    Private Function MayPlaceObject(ByRef m As Map, ByRef id As Integer, ByRef x As Integer, ByRef y As Integer) As Boolean
        If m.board(x, y).isBorder Or m.board(x, y).isAttended Then Return False
        Dim b As Location.Borders = ObjectBorders(id, x, y)
        If b.minX < 0 Or b.minY < 0 Or b.maxX > m.xSize Or b.maxY > m.ySize Then Return False
        For j As Integer = b.minY To b.maxY Step 1
            For i As Integer = b.minX To b.maxX Step 1
                If m.board(i, j).isBorder Or m.board(i, j).isAttended Or Not m.board(i, j).locID.Item(0) = id Then Return False
            Next i
        Next j
        Return True
    End Function
    Private Function MayPlaceObject(ByRef freeCell(,) As Boolean, ByRef id As Integer, ByRef x As Integer, ByRef y As Integer) As Boolean
        If Not freeCell(x, y) Then Return False
        Dim b As Location.Borders = ObjectBorders(id, x, y)
        If b.minX < 0 Or b.minY < 0 Or b.maxX > UBound(freeCell, 1) Or b.maxY > UBound(freeCell, 2) Then Return False
        For j As Integer = b.minY To b.maxY Step 1
            For i As Integer = b.minX To b.maxX Step 1
                If Not freeCell(i, j) Then Return False
            Next i
        Next j
        Return True
    End Function
    Private Sub PlaceObject(ByRef m As Map, ByRef id As Integer, ByRef x As Integer, ByRef y As Integer, ByRef GroupID As Integer)
        Dim b As Location.Borders = ObjectBorders(id, x, y)
        For j As Integer = b.minY To b.maxY Step 1
            For i As Integer = b.minX To b.maxX Step 1
                m.board(i, j).isAttended = True
            Next i
        Next j
        m.board(b.minX, b.minY).objectID = id
        m.board(b.minX, b.minY).groupID = GroupID
    End Sub
    Private Sub PlaceObject(ByRef m As Map, ByRef id As Integer, ByRef x As Integer, ByRef y As Integer, _
                            ByRef GroupID As Integer, ByRef settMap As Map.SettingsMap)
        If m.symmID < 0 Then
            Call PlaceObject(m, id, x, y, GroupID)
        Else
            Dim b As Location.Borders = ObjectBorders(id, x, y)
            Dim p(3), plist() As Point
            For k As Integer = 0 To UBound(p) Step 1
                p(k) = New Point(Integer.MaxValue, Integer.MaxValue)
            Next k
            For j As Integer = b.minY To b.maxY Step 1
                For i As Integer = b.minX To b.maxX Step 1
                    plist = symm.ApplySymm(New Point(i, j), settMap.nRaces, m, 1)
                    For k As Integer = 0 To UBound(plist) Step 1
                        m.board(plist(k).X, plist(k).Y).isAttended = True
                        If p(k).X >= plist(k).X And p(k).Y >= plist(k).Y Then p(k) = New Point(plist(k).X, plist(k).Y)
                    Next k
                Next i
            Next j
            For k As Integer = 0 To UBound(p) Step 1
                If p(k).X < Integer.MaxValue And p(k).Y < Integer.MaxValue Then
                    m.board(p(k).X, p(k).Y).objectID = id
                    m.board(p(k).X, p(k).Y).groupID = GroupID
                End If
            Next k
        End If
    End Sub
    Private Sub PlaceObject(ByRef freeCell(,) As Boolean, ByRef id As Integer, ByRef x As Integer, ByRef y As Integer)
        Dim b As Location.Borders = ObjectBorders(id, x, y)
        For j As Integer = b.minY To b.maxY Step 1
            For i As Integer = b.minX To b.maxX Step 1
                freeCell(i, j) = False
            Next i
        Next j
    End Sub

    Private Sub MakeLabyrinth(ByRef m As Map, ByVal settMap As Map.SettingsMap, ByRef Term As TerminationCondition)
        If Term.ExitFromLoops Then Exit Sub
        Dim tmpm As Map = m
        Dim TT(UBound(tmpm.Loc)) As TerminationCondition
        Dim maxTime As Long = Term.maxTime
        Parallel.For(0, tmpm.Loc.Length, _
         Sub(i As Integer)
             If Not tmpm.Loc(i).IsObtainedBySymmery Then
                 TT(i) = New TerminationCondition(maxTime)
                 Call MakeLabyrinth(tmpm, settMap, tmpm.Loc(i).ID, tmpm.symmID, TT(i))
             End If
         End Sub)
        For i As Integer = 0 To UBound(tmpm.Loc) Step 1
            If Not IsNothing(TT(i)) Then Term.ExitFromLoops = Term.ExitFromLoops Or TT(i).ExitFromLoops
        Next
        If Term.ExitFromLoops Then Exit Sub
        Term = New TerminationCondition(Term.maxTime)
        Call ConnectDisconnectedAreas(tmpm, settMap, Term)
        m = tmpm
    End Sub
    Private Sub MakeLabyrinth(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef LocId As Integer, ByRef symmID As Integer, _
                              ByRef Term As TerminationCondition)

        Dim b As New Location.Borders With {.minX = Integer.MaxValue, .minY = Integer.MaxValue, _
                                            .maxX = Integer.MinValue, .maxY = Integer.MinValue}
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID.Item(0) = LocId Then
                    b.minX = Math.Min(b.minX, x)
                    b.minY = Math.Min(b.minY, y)
                    b.maxX = Math.Max(b.maxX, x)
                    b.maxY = Math.Max(b.maxY, y)
                    m.board(x, y).isPass = False
                    If Not m.board(x, y).isBorder And Not m.board(x, y).isAttended Then
                        Dim n As Location.Borders = NearestXY(x, y, m.xSize, m.ySize, 1)
                        For j As Integer = n.minY To n.maxY Step 1
                            For i As Integer = n.minX To n.maxX Step 1
                                If Not m.board(i, j).locID.Item(0) = LocId Then
                                    m.board(x, y).Penetrable = True
                                    m.board(x, y).isPass = True
                                    i = n.maxX
                                    j = n.maxY
                                End If
                            Next i
                        Next j
                    End If
                End If
            Next x
        Next y
        For y As Integer = b.minY To b.maxY Step 1
            For x As Integer = b.minX To b.maxX Step 1
                If m.board(x, y).locID.Item(0) = LocId Then
                    m.board(x, y).isPass = m.board(x, y).isPass And m.board(x, y).Penetrable
                    If symmID > -1 Then
                        Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(p) Step 1
                            m.board(p(k).X, p(k).Y).isPass = m.board(x, y).isPass
                        Next k
                    End If
                End If
            Next x
        Next y

        Dim free(b.maxX - b.minX, b.maxY - b.minY) As Boolean
        For y As Integer = 0 To b.maxY - b.minY Step 1
            For x As Integer = 0 To b.maxX - b.minX Step 1
                If m.board(x + b.minX, y + b.minY).locID.Item(0) = LocId _
                And Not m.board(x + b.minX, y + b.minY).isAttended _
                And Not m.board(x + b.minX, y + b.minY).isBorder Then
                    free(x, y) = True
                End If
            Next x
        Next y
        Dim init() As Point = Nothing
        Dim conn()(,) As Boolean = Nothing
        Dim s As Boolean
        For y As Integer = 0 To b.maxY - b.minY Step 1
            For x As Integer = 0 To b.maxX - b.minX Step 1
                If free(x, y) And m.board(x + b.minX, y + b.minY).isPass Then
                    s = True
                    If Not IsNothing(conn) Then
                        For i As Integer = 0 To UBound(conn) Step 1
                            If conn(i)(x, y) Then
                                s = False
                                Exit For
                            End If
                        Next i
                    End If
                    If s Then
                        If IsNothing(init) Then
                            ReDim init(0), conn(0)
                        Else
                            ReDim Preserve init(init.Length), conn(conn.Length)
                        End If
                        init(UBound(init)) = New Point(x, y)
                        conn(UBound(conn)) = FindConnected(free, init(UBound(init)))
                    End If
                End If
            Next x
        Next y
        If Not IsNothing(conn) Then
            If Not IsNothing(FindDisconnected(free, conn)) Then
                Dim p As Point = FindDisconnected(free, conn)
                Throw New Exception("Какой-то объект перекрывает проход, чего не должно было получиться. Точка " & p.X + b.minX & vbTab & p.Y + b.minY)
            End If

            For i As Integer = 0 To UBound(conn) Step 1
                Call LifeAlgo(m, settMap, conn(i), init(i), New Point(b.minX, b.minY), Term)
            Next i
        End If
    End Sub
    Private Sub LifeAlgo(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef connected(,) As Boolean, _
                         ByRef init As Point, ByRef LPos As Point, ByRef Term As TerminationCondition)

        Dim xSize As Integer = UBound(connected, 1)
        Dim ySize As Integer = UBound(connected, 2)
        Dim isLifeField(xSize, ySize) As Boolean

        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If connected(x, y) And Not m.board(x + LPos.X, y + LPos.Y).Penetrable Then isLifeField(x, y) = True
            Next x
        Next y
        If m.symmID > -1 Then
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isLifeField(x, y) Then
                        Dim p() As Point = symm.ApplySymm(New Point(x + LPos.X, y + LPos.Y), settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(p) Step 1
                            Dim tx As Integer = p(k).X - LPos.X
                            Dim ty As Integer = p(k).Y - LPos.Y
                            If tx >= 0 And ty >= 0 And tx <= xSize And ty <= ySize Then isLifeField(tx, ty) = False
                        Next k
                        isLifeField(x, y) = True
                    End If
                Next x
            Next y
        End If

        Dim free(,) As Boolean = CType(connected.Clone, Boolean(,))
        Dim W(xSize, ySize) As Double
        Dim initChance As Double = 0.9
        Dim deathLower As Integer = 1
        Dim deatUpper As Integer = 3
        Dim birth As Integer = 2
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isLifeField(x, y) AndAlso rndgen.Rand(0, 1) < initChance Then free(x, y) = False
            Next x
        Next y

        Dim bW(8), dW(8), maxW As Double
        For i As Integer = 0 To UBound(bW) Step 1
            bW(i) = Math.Sqrt(1 + Math.Abs(i - birth) ^ 2)
            dW(i) = Math.Sqrt(4 + Math.Abs(deathLower - i) ^ 2 + Math.Abs(deatUpper - i) ^ 2)
            maxW = Math.Max(maxW, Math.Max(bW(i), dW(UBound(bW) - i)))
        Next i
        Dim nloops As Integer = 0
        Dim nextloop As Boolean = True
        Do While nextloop
            Term.CheckTime()
            If Term.ExitFromLoops Then Exit Sub
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isLifeField(x, y) Then
                        Dim b As Location.Borders = NearestXY(x, y, xSize, ySize, 1)
                        Dim ndead As Integer = 9 - (b.maxX - b.minX + 1) * (b.maxY - b.minY + 1)
                        If Not free(x, y) Then ndead -= 1
                        For j As Integer = b.minY To b.maxY Step 1
                            For i As Integer = b.minX To b.maxX Step 1
                                If Not connected(i, j) Or Not free(i, j) Then
                                    ndead += 1
                                End If
                            Next i
                        Next j
                        If free(x, y) Then
                            W(x, y) = bW(ndead)
                        Else
                            W(x, y) = dW(ndead)
                        End If
                    End If
                Next x
            Next y
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isLifeField(x, y) AndAlso W(x, y) > rndgen.Rand(0, maxW) Then
                        free(x, y) = Not free(x, y)
                    End If
                Next x
            Next y
            nloops += 1
            If nloops > 2 Then
                nextloop = False
                Dim tconn(,) As Boolean = FindConnected(free, init)
                nextloop = Not CompareConnection(free, connected, tconn)
            End If
        Loop

        For y As Integer = 1 To ySize - 1 Step 1
            For x As Integer = 1 To xSize - 1 Step 1
                W(x, y) = 0
                If isLifeField(x, y) And free(x, y) Then
                    Dim b As Location.Borders = NearestXY(x, y, xSize, ySize, 1)
                    Dim ndead As Integer = 0
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If Not connected(i, j) Or Not free(i, j) Then
                                ndead += 1
                            End If
                        Next i
                    Next j
                    If ndead < 1 And ndead < 5 Then
                        Dim t As Integer = 0
                        Dim i As Integer
                        Dim j As Integer
                        For k As Integer = -1 To 1 Step 2
                            i = x + k
                            j = y
                            If Not connected(i, j) Or Not free(i, j) Then t += 1
                            i = x
                            j = y + k
                            If Not connected(i, j) Or Not free(i, j) Then t += 1
                        Next k
                        If ndead = t Then W(x, y) = 0.3 + 0.1 * CDbl(ndead)
                    End If
                End If
            Next x
        Next y
        For y As Integer = 1 To ySize - 1 Step 1
            For x As Integer = 1 To xSize - 1 Step 1
                If W(x, y) > 0 AndAlso W(x, y) > rndgen.Rand(0, maxW) Then
                    free(x, y) = Not free(x, y)
                    Dim tconn(,) As Boolean = FindConnected(free, init)
                    If Not CompareConnection(free, connected, tconn) Then
                        free(x, y) = Not free(x, y)
                    End If
                End If
            Next x
        Next y

        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isLifeField(x, y) Then
                    If m.symmID > -1 Then
                        Dim p() As Point = symm.ApplySymm(New Point(x + LPos.X, y + LPos.Y), settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(p) Step 1
                            m.board(p(k).X, p(k).Y).isBorder = Not free(x, y)
                        Next k
                    Else
                        m.board(x + LPos.X, y + LPos.Y).isBorder = Not free(x, y)
                    End If
                End If
            Next x
        Next y

    End Sub
    Private Function CompareConnection(ByRef freeCells(,) As Boolean, ByRef connected1(,) As Boolean, _
                                       ByRef connected2(,) As Boolean) As Boolean
        Dim xSize As Integer = UBound(freeCells, 1)
        Dim ySize As Integer = UBound(freeCells, 2)
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If freeCells(x, y) And (Not connected1(x, y) = connected2(x, y)) Then Return False
            Next x
        Next y
        Return True
    End Function

End Class

Public Class SymmetryOperations

    ''' <summary>Возвращает точку, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(mapXSize - p.X, mapYSize - p.Y)
    End Function
    ''' <summary>Возвращает точку, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef p As Point, ByRef m As Map) As Point
        Return L2(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = L.Copy
        Call r.L2Rotation(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef p As Point, ByRef mapSize As Integer) As Point
        Return New Point(p.Y, mapSize - p.X)
    End Function
    ''' <summary>Возвращает точку, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef p As Point, ByRef m As Map) As Point
        Return L4(p, m.xSize)
    End Function
    ''' <summary>Возвращает локацию, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = L.Copy
        Call r.L4Rotation(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(p.X, mapYSize - p.Y)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef p As Point, ByRef m As Map) As Point
        Return xM(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = L.Copy
        Call r.xReflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(mapXSize - p.X, p.Y)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef p As Point, ByRef m As Map) As Point
        Return yM(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = L.Copy
        Call r.yReflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef p As Point) As Point
        Return New Point(p.Y, p.X)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef p As Point, ByRef m As Map) As Point
        Return xy1M(p)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = L.Copy
        Call r.xy1Reflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef p As Point, ByRef mapSize As Integer) As Point
        Return New Point(mapSize - p.Y, mapSize - p.X)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef p As Point, ByRef m As Map) As Point
        Return xy2M(p, m.xSize)
    End Function
    ''' <summary>Возвращает локация, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef L As Location, ByRef m As Map) As Location
        Dim r As Location = L.Copy
        Call r.xy2Reflection(m, Me)
        r.IsObtainedBySymmery = True
        Return r
    End Function

    ''' <summary>Применяет одну из операций симметрии, разрешенную для двух игроков</summary>
    ''' <param name="ID">id операции</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function TwoPlayersSymm(ByRef p As Point, ByRef m As Map, _
                                    ByRef id As Integer, ByRef minSqDist As Integer) As Point()
        Dim pp As Point
        If id = 0 Then
            pp = L2(p, m)
        ElseIf id = 1 Then
            pp = xM(p, m)
        ElseIf id = 2 Then
            pp = yM(p, m)
        ElseIf id = 3 Then
            pp = xy1M(p)
        ElseIf id = 4 Then
            pp = xy2M(p, m)
        Else
            Throw New Exception("Неожиданный id операции симметрии")
        End If
        Return CheckPointsDist(New Point() {p, pp}, minSqDist)
    End Function
    ''' <summary>Применяет одну из операций симметрии, разрешенную для двух игроков</summary>
    ''' <param name="ID">id операции</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function TwoPlayersSymm(ByRef L As Location, ByRef m As Map, _
                                    ByRef id As Integer, ByRef minSqDist As Integer) As Location()
        Dim ll As Location
        If id = 0 Then
            ll = L2(L, m)
        ElseIf id = 1 Then
            ll = xM(L, m)
        ElseIf id = 2 Then
            ll = yM(L, m)
        ElseIf id = 3 Then
            ll = xy1M(L, m)
        ElseIf id = 4 Then
            ll = xy2M(L, m)
        Else
            Throw New Exception("Неожиданный id операции симметрии")
        End If
        Return CheckPointsDist(New Location() {L, ll}, minSqDist)
    End Function
    Private Function CheckPointsDist(ByRef p() As Point, ByRef minSqDist As Integer) As Point()
        Dim dx, dy As Integer
        Dim ok As Boolean = True
        For i As Integer = 0 To UBound(p) - 1 Step 1
            For j As Integer = i + 1 To UBound(p) Step 1
                dx = p(i).X - p(j).X
                dy = p(i).Y - p(j).Y
                If dx * dx + dy * dy < minSqDist Then
                    ok = False
                    i = p.Length
                    Exit For
                End If
            Next j
        Next i
        If ok Then
            Return p
        Else
            Dim x, y As Integer
            For i As Integer = 0 To UBound(p) Step 1
                x += p(i).X
                y += p(i).Y
            Next i
            x = CInt(x / p.Length)
            y = CInt(y / p.Length)
            Return New Point() {New Point(x, y)}
        End If
    End Function
    Private Function CheckPointsDist(ByRef L() As Location, ByRef minSqDist As Integer) As Location()
        Dim dx, dy As Integer
        Dim ok As Boolean = True
        Dim minID As Integer = Integer.MaxValue
        For i As Integer = 0 To UBound(L) Step 1
            If minID > L(i).ID Then minID = L(i).ID
        Next i
        For i As Integer = 0 To UBound(L) - 1 Step 1
            For j As Integer = i + 1 To UBound(L) Step 1
                dx = L(i).pos.X - L(j).pos.X
                dy = L(i).pos.Y - L(j).pos.Y
                If dx * dx + dy * dy < minSqDist Then
                    ok = False
                    i = L.Length
                    Exit For
                End If
            Next j
        Next i
        If ok Then
            For i As Integer = 0 To UBound(L) Step 1
                L(i).ID = minID + i
            Next i
            Return L
        Else
            Dim x, y As Integer
            Dim a, b, alpha As Double
            Dim isSymm As Boolean = True
            For i As Integer = 0 To UBound(L) Step 1
                x += L(i).pos.X
                y += L(i).pos.Y
                a += L(i).gASize
                b += L(i).gBSize
                alpha += L(i).gAlpha
                If Not L(i).IsObtainedBySymmery Then isSymm = False
            Next i
            x = CInt(x / L.Length)
            y = CInt(y / L.Length)
            a /= L.Length
            b /= L.Length
            alpha /= L.Length
            Dim res As New Location(New Point(x, y), a, b, alpha, minID, isSymm)
            Return New Location() {res}
        End If
    End Function

    ''' <summary>Применяет одну из операций симметрии, разрешенную для четырех игроков</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function FourPlayersSymm(ByRef p As Point, ByRef m As Map, ByRef minSqDist As Integer) As Point()

        Dim res() As Point
        Dim n As Integer = -1
        If m.symmID = 0 Then
            ReDim res(3)
            res(0) = New Point(p.X, p.Y)
            For i As Integer = 1 To UBound(res) Step 1
                res(i) = L4(res(i - 1), m)
            Next i
            Return CheckPointsDist(res, minSqDist)
        ElseIf m.symmID = 1 Or m.symmID = 2 Then
            Dim op1, op2 As Integer
            If m.symmID = 1 Then
                op1 = 1
                op2 = 2
            Else
                op1 = 3
                op2 = 4
            End If
            Dim pp() As Point = TwoPlayersSymm(p, m, op1, minSqDist)
            Dim r(UBound(pp))() As Point
            For i As Integer = 0 To UBound(pp) Step 1
                r(i) = TwoPlayersSymm(pp(i), m, op2, minSqDist)
                n += r(i).Length
            Next i
            ReDim res(n)
            n = -1
            For i As Integer = 0 To UBound(r) Step 1
                For j As Integer = 0 To UBound(r(i)) Step 1
                    n += 1
                    res(n) = r(i)(j)
                Next j
            Next i
            Return res
        Else
            Throw New Exception("Неожиданный id операции симметрии")
            Return Nothing
        End If
    End Function
    ''' <summary>Применяет одну из операций симметрии, разрешенную для четырех игроков</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function FourPlayersSymm(ByRef L As Location, ByRef m As Map, ByRef minSqDist As Integer) As Location()

        Dim res() As Location
        Dim n As Integer = -1
        If m.symmID = 0 Then
            ReDim res(3)
            res(0) = L.Copy
            For i As Integer = 1 To UBound(res) Step 1
                res(i) = L4(res(i - 1), m)
            Next i
            Return CheckPointsDist(res, minSqDist)
        ElseIf m.symmID = 1 Or m.symmID = 2 Then
            Dim op1, op2 As Integer
            If m.symmID = 1 Then
                op1 = 1
                op2 = 2
            Else
                op1 = 3
                op2 = 4
            End If
            Dim ll() As Location = TwoPlayersSymm(L, m, op1, minSqDist)
            Dim r(UBound(ll))() As Location
            For i As Integer = 0 To UBound(ll) Step 1
                r(i) = TwoPlayersSymm(ll(i), m, op2, minSqDist)
                n += r(i).Length
            Next i
            ReDim res(n)
            n = -1
            For i As Integer = 0 To UBound(r) Step 1
                For j As Integer = 0 To UBound(r(i)) Step 1
                    n += 1
                    res(n) = r(i)(j)
                Next j
            Next i
            Return CheckPointsDist(res, minSqDist)
        Else
            Throw New Exception("Неожиданный id операции симметрии")
            Return Nothing
        End If
    End Function

    ''' <summary>Возвращает список возможных операций симметрии.
    ''' Для двух рас: 0 - L2, 1 - xM, 2 - yM, 3 - xy1M, 4 - xy2M.
    ''' Для четырех рас: 0 - L4, 1 - xM+yM, 2 - xy1M+xy2M</summary>
    ''' <param name="nRaces">Количество рас</param>
    ''' <param name="xSize">Размер по оси X</param>
    ''' <param name="ySize">Размер по оси Y</param>
    Public Function PossibleOperationsList(ByRef nRaces As Integer, ByRef xSize As Integer, ByRef ySize As Integer) As List(Of Integer)
        Dim res As New List(Of Integer)
        If nRaces = 2 Then
            res.AddRange(New Integer() {0, 0, 1, 2})
            If xSize = ySize Then res.AddRange(New Integer() {3, 4})
        ElseIf nRaces = 4 Then
            res.Add(1)
            If xSize = ySize Then res.AddRange(New Integer() {0, 2})
        End If
        Return res
    End Function
    ''' <summary>Возвращает список возможных операций симметрии.
    ''' Для двух рас: 0 - L2, 1 - xM, 2 - yM, 3 - xy1M, 4 - xy2M.
    ''' Для четырех рас: 0 - L4, 1 - xM+yM, 2 - xy1M+xy2M</summary>
    ''' <param name="nRaces">Количество рас</param>
    ''' <param name="m">Карта. Важен только размер</param>
    Public Function PossibleOperationsList(ByRef nRaces As Integer, ByRef m As Map) As List(Of Integer)
        Return PossibleOperationsList(nRaces, m.xSize, m.ySize)
    End Function

    ''' <summary>Применяет одну из операций симметрии. ID симметрии в переменной m.
    ''' Для двух рас: 0 - L2, 1 - xM, 2 - yM, 3 - xy1M, 4 - xy2M.
    ''' Для четырех рас: 0 - L4, 1 - xM+yM, 2 - xy1M+xy2M</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Public Function ApplySymm(ByRef p As Point, ByRef nRaces As Integer, ByRef m As Map, _
                              ByRef minSqDist As Integer) As Point()
        If nRaces = 2 Then
            Return TwoPlayersSymm(p, m, m.symmID, minSqDist)
        ElseIf nRaces = 4 Then
            Return FourPlayersSymm(p, m, minSqDist)
        Else
            Return Nothing
        End If
    End Function

    ''' <summary>Применяет одну из операций симметрии. ID симметрии в переменной m.
    ''' Для двух рас: 0 - L2, 1 - xM, 2 - yM, 3 - xy1M, 4 - xy2M.
    ''' Для четырех рас: 0 - L4, 1 - xM+yM, 2 - xy1M+xy2M</summary>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Public Function ApplySymm(ByRef L As Location, ByRef nRaces As Integer, ByRef m As Map, _
                              ByRef minSqDist As Integer) As Location()
        If nRaces = 2 Then
            Return TwoPlayersSymm(L, m, m.symmID, minSqDist)
        ElseIf nRaces = 4 Then
            Return FourPlayersSymm(L, m, minSqDist)
        Else
            Return Nothing
        End If
    End Function
End Class

Public Class Location

    ''' <summary>Номер локации, больше ноля</summary>
    Public ID As Integer
    ''' <summary>Положение локации</summary>
    Public pos As Point
    Private invSqA, invSqB, cos, sin As Double
    Private Asize, Bsize As Double
    Private invSigmaA, invSigmaB As Double
    Private alpha As Double
    ''' <summary>True, если получена операцией симметрии</summary>
    Public IsObtainedBySymmery As Boolean

    Friend Structure Borders
        Dim maxX, minX, maxY, minY As Integer
    End Structure

    ''' <summary>Возвращает угол поворота локации</summary>
    Public Function gAlpha() As Double
        Return alpha
    End Function
    ''' <summary>Возвращает длину оси А</summary>
    Public Function gASize() As Double
        Return Asize
    End Function
    ''' <summary>Возвращает длину оси B</summary>
    Public Function gBSize() As Double
        Return Bsize
    End Function

    ''' <param name="p">Положение локации</param>
    ''' <param name="a">Половина ширины</param>
    ''' <param name="b">Половина высоты</param>
    ''' <param name="angle">Угол наклона</param>
    ''' <param name="i">Номер локации, больше ноля</param>
    Public Sub New(ByRef p As Point, ByVal a As Double, ByVal b As Double, ByVal angle As Double, ByVal i As Integer, Optional ByVal symmetred As Boolean = False)
        pos = New Point(p.X, p.Y)
        invSigmaA = Math.Sqrt(0.5) * 0.5 / a
        invSigmaB = Math.Sqrt(0.5) * 0.5 / b
        invSqA = 1 / (a * a)
        invSqB = 1 / (b * b)
        Asize = a
        Bsize = b
        ID = i
        alpha = angle
        Call CosSinCalc()
        IsObtainedBySymmery = symmetred
    End Sub

    Public Function Copy() As Location
        Return New Location(pos, Asize, Bsize, alpha, ID, IsObtainedBySymmery)
    End Function

    Friend Shared Function FindLocIDByPosition(ByRef m As Map, ByRef p As Point) As Integer
        For j As Integer = 0 To UBound(m.Loc) Step 1
            If Math.Abs(m.Loc(j).pos.X - p.X) < 2 And Math.Abs(m.Loc(j).pos.Y - p.Y) < 2 Then Return j
            If j = UBound(m.Loc) Then Throw New Exception("Не могу найти локацию по координате")
        Next j
        Return -1
    End Function

    Private Sub CosSinCalc()
        cos = Math.Cos(alpha)
        sin = Math.Sin(alpha)
    End Sub

    Friend Sub L2Rotation(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha += Math.PI
        If alpha >= 2 * Math.PI Then alpha -= 2 * Math.PI
        Call CosSinCalc()
        pos = symm.L2(pos, m)
    End Sub
    Friend Sub L4Rotation(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha += 0.5 * Math.PI
        If alpha >= 2 * Math.PI Then alpha -= 2 * Math.PI
        Call CosSinCalc()
        pos = symm.L4(pos, m)
    End Sub
    Friend Sub xReflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = 2 * Math.PI - alpha
        Call CosSinCalc()
        pos = symm.xM(pos, m)
    End Sub
    Friend Sub yReflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.yM(pos, m)
    End Sub
    Friend Sub xy1Reflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = 0.5 * Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.xy1M(pos, m)
    End Sub
    Friend Sub xy2Reflection(ByRef m As map, ByRef symm As SymmetryOperations)
        alpha = 1.5 * Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.xy2M(pos, m)
    End Sub

    Friend Function IsInside(ByRef X As Integer, ByRef Y As Integer) As Boolean
        Dim dx As Integer = X - pos.X
        Dim dy As Integer = Y - pos.Y
        Dim x1 As Double = dx * cos
        Dim x2 As Double = dy * sin
        Dim y1 As Double = -dx * sin
        Dim y2 As Double = dy * cos
        x1 += x2
        y1 += y2
        If x1 * x1 * invSqA + y1 * y1 * invSqB > 1 Then
            Return False
        Else
            Return True
        End If
    End Function
    Friend Function IsInside(ByRef P As Point) As Boolean
        Return IsInside(P.X, P.Y)
    End Function

    Friend Function XYborders(ByRef maxX As Integer, ByRef maxY As Integer) As Borders
        Return XYborders(maxX, maxY, 0, 0)
    End Function
    Friend Function XYborders(ByRef maxX As Integer, ByRef maxY As Integer, ByRef minX As Integer, ByRef minY As Integer) As Borders
        Dim dx As Integer = CInt(Math.Ceiling(Math.Abs(Asize * cos) + Math.Abs(Bsize * sin)))
        Dim dy As Integer = CInt(Math.Ceiling(Math.Abs(Asize * sin) + Math.Abs(Bsize * cos)))
        Return New Borders With {.minX = Math.Max(pos.X - dx, minX), _
                                 .maxX = Math.Min(pos.X + dx, maxX), _
                                 .miny = Math.Max(pos.Y - dy, minX), _
                                 .maxy = Math.Min(pos.Y + dy, maxY)}
    End Function

    Friend Function pWeight(ByRef X As Integer, ByRef Y As Integer) As Double
        Dim dx As Integer = X - pos.X
        Dim dy As Integer = Y - pos.Y
        Dim d1 As Double = dx * cos + dy * sin
        Dim d2 As Double = -dx * sin + dy * cos
        Return Math.Exp(-((dx * invSigmaA) ^ 2) - ((dy * invSigmaB) ^ 2))
    End Function
    Friend Function pWeight(ByRef P As Point) As Double
        Return pWeight(P.X, P.Y)
    End Function
    Private Function Gauss(ByRef dX As Double, ByRef sigma As Double) As Double
        Return Math.Exp(-0.5 * (dX / sigma) ^ 2)
    End Function

    Friend Shared Function GenLocSize(ByRef sett As Map.SettingsLoc, ByRef id As Integer, _
                                      ByRef rndgen As RndValueGen, ByRef minLocationRadiusAtAll As Double) As Location
        Dim r, e, a As Double
        If sett.maxRadiusDispersion >= 1 Or sett.maxRadiusDispersion < 0 Then Throw New Exception("Invalid radius dispersion: " & sett.maxRadiusDispersion & "(>=1 or <0)")
        If sett.maxEccentricityDispersion >= 1 Or sett.maxEccentricityDispersion < 0 Then Throw New Exception("Invalid eccentricity dispersion: " & sett.maxEccentricityDispersion & "(>=1 or <0)")

        r = rndgen.PRand(1 - sett.maxRadiusDispersion, 1 + sett.maxRadiusDispersion) * sett.AverageRadius
        e = rndgen.PRand(1 - sett.maxEccentricityDispersion, 1 + sett.maxEccentricityDispersion)
        a = rndgen.PRand(0, Math.PI)
        r = Math.Max(r, minLocationRadiusAtAll)
        Return New Location(New Point(0, 0), r * e, r / e, a, id)
    End Function

End Class

Public Class ArrayZoom

    ''' <summary>Увеличит массив в multiplicator раз</summary>
    Public Function Zoom(ByVal grid(,) As Integer, ByVal multiplicator As Integer) As Integer(,)
        Dim imgXSize As Integer = UBound(grid, 1)
        Dim imgYSize As Integer = UBound(grid, 2)
        Dim res((imgXSize + 1) * multiplicator - 1, (imgYSize + 1) * multiplicator - 1) As Integer
        Parallel.For(0, imgXSize + 1, _
         Sub(i As Integer)
             Dim x, y, xm, ym As Integer
             xm = i * multiplicator
             For J As Integer = 0 To imgYSize Step 1
                 ym = J * multiplicator
                 For i1 As Integer = 0 To multiplicator - 1 Step 1
                     x = xm + i1
                     For j1 As Integer = 0 To multiplicator - 1 Step 1
                         y = ym + j1
                         res(x, y) = grid(i, J)
                     Next j1
                 Next i1
             Next J
         End Sub)
        Return res
    End Function

    ''' <summary>Определит, во сколько раз нужно увеличить массив</summary>
    ''' <param name="size">max(ширина,высота)</param>
    ''' <param name="desiredsize">максимальный размер</param>
    Public Function CalcMultiplicator(ByRef size As Integer, ByRef desiredsize As Integer) As Integer
        Dim maxSize As Integer = desiredsize
        Dim m As Integer = 0
        Do While (m + 1) * size <= maxSize
            m += 1
        Loop
        Return Math.Max(m, 1)
    End Function

End Class

Public Class ColorSelector

    Private ColorCubeSize As Integer = 255
    Private maxBrightness As Integer = CInt(CDbl(3 * ColorCubeSize) * 0.9)
    Private minBrightness As Integer = CInt(CDbl(3 * ColorCubeSize) * 0.1)

    ''' <summary>Подберет цвета в зависимости от значений в ячейках</summary>
    Public Function MakeColorMap(ByRef grid(,) As Integer) As Color(,)

        Dim imgXSize As Integer = UBound(grid, 1)
        Dim imgYSize As Integer = UBound(grid, 2)
        Dim n As Integer = 0
        For i As Integer = 0 To imgXSize Step 1
            For j As Integer = 0 To imgYSize Step 1
                If n < grid(i, j) Then n = grid(i, j)
            Next j
        Next i
        Dim colorsmap(imgXSize, imgYSize) As Color

        Dim palette(n) As Color
        Dim xSize, ySize, zSize As Integer
        Dim p As Integer = 0
        Do While p < n
            p = 0
            If xSize * ySize * zSize < n Then xSize += 1
            If xSize * ySize * zSize < n Then ySize += 1
            If xSize * ySize * zSize < n Then zSize += 1
            If Not xSize * ySize * zSize < n Then
                Dim dx_ As Double = CoordinateStep(ColorCubeSize, xSize)
                Dim dy_ As Double = CoordinateStep(ColorCubeSize, ySize)
                Dim dz_ As Double = CoordinateStep(ColorCubeSize, zSize)
                For k As Integer = 0 To zSize - 1 Step 1
                    Dim z As Integer = ColorCoordinate(k, dz_, zSize, ColorCubeSize)
                    For j As Integer = 0 To ySize - 1 Step 1
                        Dim y As Integer = ColorCoordinate(j, dy_, ySize, ColorCubeSize)
                        Dim s As Integer = y + z
                        For i As Integer = 0 To xSize - 1 Step 1
                            Dim x As Integer = ColorCoordinate(i, dx_, xSize, ColorCubeSize)
                            If UseColor(s + x) Then p += 1
                        Next i
                    Next j
                Next k
                If p < n Then
                    Dim m As Integer = Math.Min(xSize, Math.Min(ySize, zSize))
                    If m = xSize Then
                        xSize += 1
                    ElseIf m = ySize Then
                        ySize += 1
                    Else
                        zSize += 1
                    End If
                End If
            End If
        Loop

        Dim col(3 * ColorCubeSize) As Collections.Generic.List(Of Color)
        Dim u As Integer = UBound(col)
        For i As Integer = 0 To u Step 1
            col(i) = New Collections.Generic.List(Of Color)
        Next i
        Dim dx As Double = CoordinateStep(ColorCubeSize, xSize)
        Dim dy As Double = CoordinateStep(ColorCubeSize, ySize)
        Dim dz As Double = CoordinateStep(ColorCubeSize, zSize)
        For k As Integer = 0 To zSize - 1 Step 1
            Dim z As Integer = ColorCoordinate(k, dz, zSize, ColorCubeSize)
            For j As Integer = 0 To ySize - 1 Step 1
                Dim y As Integer = ColorCoordinate(j, dy, ySize, ColorCubeSize)
                Dim s As Integer = y + z
                For i As Integer = 0 To xSize - 1 Step 1
                    Dim x As Integer = ColorCoordinate(i, dx, xSize, ColorCubeSize)
                    If UseColor(s + x) Then col(s + x).Add(Color.FromArgb(x, y, z))
                Next i
            Next j
        Next k

        Dim column, row As Integer
        column = 0 : row = u
        For i As Integer = 1 To n Step 1
            Dim c As Boolean = True
            Do While c
                If col(row).Count = column Then
                    row -= 1
                    column = 0
                Else
                    palette(i) = col(row).Item(column)
                    column += 1
                    c = False
                End If
            Loop
        Next i

        Dim tmesh(,) As Integer = CType(grid.Clone, Integer(,))
        Parallel. _
         For(0, imgXSize + 1, _
         Sub(x1 As Integer)
             Dim id As Integer
             For y1 As Integer = 0 To imgYSize Step 1
                 id = tmesh(x1, y1)
                 If id = 0 Then
                     colorsmap(x1, y1) = Color.Black
                 Else
                     colorsmap(x1, y1) = palette(id)
                 End If
             Next y1
         End Sub)

        Return colorsmap
    End Function
    Private Function CoordinateStep(ByRef A As Integer, ByRef ItemsCount As Integer) As Double
        If ItemsCount > 1 Then
            Return CDbl(A) / CDbl(ItemsCount - 1)
        Else
            Return CDbl(A)
        End If
    End Function
    Private Function ColorCoordinate(ByRef n As Integer, ByRef dn As Double, _
                                     ByRef ItemsCount As Integer, ByRef A As Integer) As Integer
        If ItemsCount > 1 Then
            If n < ItemsCount Then
                Return CInt(CDbl(n) * dn)
            Else
                Return A
            End If
        Else
            Return CInt(0.5 * dn)
        End If
    End Function
    Private Function UseColor(ByRef RGBsum As Integer) As Boolean
        If RGBsum <= maxBrightness AndAlso RGBsum >= minBrightness Then
            Return True
        Else
            Return False
        End If
    End Function

End Class

Class TerminationCondition
    Friend maxTime As Long
    Dim startTime As Long
    Public ExitFromLoops As Boolean

    Public Sub New(ByVal maxT As Integer)
        Call init(CLng(maxT))
    End Sub
    Public Sub New(ByVal maxT As Long)
        Call init(maxT)
    End Sub
    Private Sub init(ByRef maxT As Long)
        maxTime = maxT
        startTime = CLng(Environment.TickCount)
        ExitFromLoops = False
    End Sub

    Public Sub CheckTime()
        Dim T As Long = CLng(Environment.TickCount)
        Dim d As Long = DeltaTime(startTime, T)
        If d > maxTime Then ExitFromLoops = True
    End Sub

    Private Function DeltaTime(ByRef startT As Long, ByRef endT As Long) As Long
        If endT >= startT Then
            Return endT - startT
        Else
            Return CLng(Integer.MaxValue) - startT + endT - CLng(Integer.MinValue)
        End If
    End Function

End Class

Public Class AttendedObject
    ''' <summary>Номер записи в массиве со всеми посещаемыми объектами</summary>
    Public ReadOnly TypeID As Integer
    ''' <summary>Длина стороны объекта</summary>
    Public ReadOnly Size As Integer
    ''' <summary>Название объекта</summary>
    Public ReadOnly Name As String
    ''' <summary>Нужно ли размещать охраняющий отряд</summary>
    Public ReadOnly hasExternalGuard As Boolean
    ''' <summary>Площадь, которую нужно выделить под объект</summary>
    Friend ReadOnly Area As Integer
    ''' <summary>Положение объекта по X и Y относительно положения области, выделенной под него</summary>
    Friend ReadOnly dxy As Integer

    Public Sub New(ByVal objName As String, ByVal objSize As Integer, ByVal objTypeID As Integer, _
                   Optional ByVal objHasExternalGuard As Boolean = False)
        Name = objName
        Size = objSize
        TypeID = objTypeID
        hasExternalGuard = objHasExternalGuard

        dxy = 1
        If hasExternalGuard Then dxy += 1
        Area = CInt((Size + 2 * dxy) ^ 2)
    End Sub
End Class

Public Class Map
    ''' <summary>Список локаций. Первые в списке - стартовые по числу играбельных рас на карте</summary>
    Public Loc As Location()
    ''' <summary>Поле карты, содержащее свойства каждой клетки</summary>
    Public board(,) As Cell
    ''' <summary>Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 23)</summary>
    Public ReadOnly xSize As Integer
    ''' <summary>Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 47)</summary>
    Public ReadOnly ySize As Integer
    ''' <summary>Идентификатор симметрии, применяемой при генерации</summary>
    Public ReadOnly symmID As Integer
    ''' <summary>Желаемые статы для каждой группы стэков. Индексы групп (key) хранятся в board(,).groupID. В этом словаре могут присутствовать и отрицательные ключи - для внутренней охраны городов. Положительный ключ - внешняя охрана, отрицательный, но равный по модулю - внутренняя охрана того же города, при этом внутренняя охрана не всегда может присутствовать</summary>
    Public groupStats As Dictionary(Of Integer, AllDataStructues.DesiredStats)
    ''' <summary>Какие этапы закончены</summary>
    Public complited As ComplitedSteps

    ''' <param name="xDim">Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 23)</param>
    ''' <param name="yDim">Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 47)</param>
    ''' <param name="SymmApplied">Идентификатор симметрии, применяемой при генерации</param>
    Public Sub New(ByVal xDim As Integer, ByVal yDim As Integer, ByVal SymmApplied As Integer)
        xSize = xDim
        ySize = yDim
        symmID = SymmApplied
        ReDim board(xSize, ySize)
        For x As Integer = 0 To xSize Step 1
            For y As Integer = 0 To ySize Step 1
                board(x, y).locID = New List(Of Integer)
            Next y
        Next x
    End Sub

    Public Structure Cell
        ''' <summary>ID локаций, с которыми связана эта клетка</summary>
        Dim locID As List(Of Integer)
        ''' <summary>True, если на клетке должен стоять непроходимый непосещаемый объект</summary>
        Dim isBorder As Boolean
        ''' <summary>True, если клетка обязательно должна быть проходимой</summary>
        Dim Penetrable As Boolean
        ''' <summary>True, если клетка является частью прохода между локациями</summary>
        Dim isPass As Boolean
        ''' <summary>True, если клетка находится под посещаемым объектом</summary>
        Dim isAttended As Boolean
        ''' <summary>True, отряд является внешней или внутренней охраной посещаемого объекта</summary>
        Dim isObjectGuard As Boolean
        ''' <summary>True, если на клетке нужно разместить обычный отряд</summary>
        Dim GuardLoc As Boolean
        ''' <summary>True, если на клетке нужно разместить отряд, охраняющий проход в соседнюю локацию</summary>
        Dim PassGuardLoc As Boolean
        ''' <summary>Если клетка является углом посещаемого объекта c наименьшей координатой по X и Y, то здесь хранится ID объекта</summary>
        Dim objectID As Integer
        ''' <summary>Если клетка является углом объекта c наименьшей координатой по X и Y, то здесь хранится ID объекта, как он записан в ресурсах игры</summary>
        Dim objectName As String
        ''' <summary>Если клетка является углом посещаемого объекта c наименьшей координатой по X и Y, то здесь хранится подтип объекта</summary>
        Dim Subtype As Integer
        ''' <summary>Для объектов с одинаковым ID выставляются одинаковые параметры генерации отрядов и лута или одинаковый класс.
        ''' При необходимости выставляются одинаковые отряды и лут.</summary>
        Dim groupID As Integer
        ''' <summary>Расы, допустимые для объекта, занимающего эту клетку</summary>
        Dim objRace As List(Of Integer)
        ''' <summary>Расы, допустимые для воинов отряда, занимающего эту клетку</summary>
        Dim stackRace As List(Of Integer)
        ''' <summary>True, если на клетке вода</summary>
        Dim isWater As Boolean
        ''' <summary>True, если на клетке дорога</summary>
        Dim isRoad As Boolean
        ''' <summary>True, если на клетке лес</summary>
        Dim isForest As Boolean
    End Structure

    Public Structure ComplitedSteps
        Dim LoationsCreation_Done As Boolean
        Dim MeshTestI_Done As Boolean
        Dim StacksPlacing_Done As Boolean
        Dim MeshTestII_Done As Boolean
        Dim StacksDesiredStatsGen_Done As Boolean
        Dim WaterCreation_Done As Boolean
        Dim StacksRaceGen_Done As Boolean
        Dim ImpenetrableObjectsPlacing_Done As Boolean
        Dim PenetrableObjectsPlacing_Done As Boolean
    End Structure

    Public Structure SettingsLoc
        ''' <summary>Средний радиус локаций</summary>
        Dim AverageRadius As Integer
        ''' <summary>Локации будут в форме эллипсов со случайным эксцентриситетом от (1-D)/(1+D) до (1+D)/(1-D)</summary>
        Dim maxEccentricityDispersion As Double
        ''' <summary>При достаточном количестве места будут создаваться локации размером от (1-D)*R до (1+D)*R.
        ''' Когда свободного места станет недостаточно R начнет постепенно уменьшаться до половины от начального значения</summary>
        Dim maxRadiusDispersion As Double

        '''<summary>Количество золотых шахт на локацию</summary>
        Dim maxGoldMines As Double
        '''<summary>Количество источников маны на локацию</summary>
        Dim maxManaSources As Double
        '''<summary>Количество нейтральных городов на локацию</summary>
        Dim maxCities As Double
        '''<summary>Количество торговцев на локацию</summary>
        Dim maxVendors As Double
        '''<summary>Количество лагерей наемников на локацию</summary>
        Dim maxMercenaries As Double
        '''<summary>Количество башен мага на локацию</summary>
        Dim maxMages As Double
        '''<summary>Количество тренеров на локацию</summary>
        Dim maxTrainers As Double
        '''<summary>Количество руин на локацию</summary>
        Dim maxRuins As Double

        '''<summary>Минимальное расстояние между отрядами</summary>
        Dim minStackToStackDist As Double

        '''<summary>Примерное количество опыта за убийство всех отрядов в локации</summary>
        Dim expAmount As Double

        '''<summary>Минимальный уровень заклинаний в лавке мага</summary>
        Dim mageSpellsMaxLevel As Integer
        '''<summary>Максимальный уровень заклинаний в лавке мага</summary>
        Dim mageSpellsMinLevel As Integer
        '''<summary>Количество заклинаний в лавке мага</summary>
        Dim mageSpellsCount As Integer
        '''<summary>Могут ли встречаться в лавке мага заклинания, действующие на всю карту</summary>
        Dim mageGlobalSpellsEnabled As Boolean

        '''<summary>Максимальная планка опыта у маленьких наемников (для больших в два раза выше)</summary>
        Dim mercenariesMaxExpBar As Integer
        '''<summary>Минимальная планка опыта у маленьких наемников (для больших в два раза выше)</summary>
        Dim mercenariesMinExpBar As Integer
        '''<summary>Количество наемников в лагере</summary>
        Dim mercenariesCount As Integer

        '''<summary>Максимальная стоимость предмета у торговца</summary>
        Dim merchMaxItemCost As Integer
        '''<summary>Минимальная стоимость предмета у торговца</summary>
        Dim merchMinItemCost As Integer
        '''<summary>Полная стоимость лута у торговца</summary>
        Dim merchItemsCost As Integer

        Private Checked As Boolean
        ''' <summary>Проверит корректность параметров. Вернет пустое сообщение, если все нормально</summary>
        Public Function Check() As String

            Dim fields As New Dictionary(Of String, String)

            fields.Add("AverageRadius", AverageRadius.ToString)
            fields.Add("maxEccentricityDispersion", maxEccentricityDispersion.ToString)
            fields.Add("maxRadiusDispersion", maxRadiusDispersion.ToString)
            fields.Add("maxGoldMines", maxGoldMines.ToString)
            fields.Add("maxManaSources", maxManaSources.ToString)
            fields.Add("maxCities", maxCities.ToString)
            fields.Add("maxVendors", maxVendors.ToString)
            fields.Add("maxMercenaries", maxMercenaries.ToString)
            fields.Add("maxMages", maxMages.ToString)
            fields.Add("maxTrainers", maxTrainers.ToString)
            fields.Add("maxRuins", maxRuins.ToString)
            fields.Add("minStackToStackDist", minStackToStackDist.ToString)
            fields.Add("expAmount", expAmount.ToString)
            fields.Add("mageSpellsMaxLevel", mageSpellsMaxLevel.ToString)
            fields.Add("mageSpellsMinLevel", mageSpellsMinLevel.ToString)
            fields.Add("mageSpellsCount", mageSpellsCount.ToString)
            'fields.Add("mageGlobalSpellsEnabled", mageGlobalSpellsEnabled.ToString)
            fields.Add("mercenariesMaxExpBar", mercenariesMaxExpBar.ToString)
            fields.Add("mercenariesMinExpBar", mercenariesMinExpBar.ToString)
            fields.Add("mercenariesCount", mercenariesCount.ToString)
            fields.Add("merchMaxItemCost", merchMaxItemCost.ToString)
            fields.Add("merchMinItemCost", merchMinItemCost.ToString)
            fields.Add("merchItemsCost", merchItemsCost.ToString)

            Dim msg As String = ""
            Dim less0 As New List(Of String)
            Dim equal0 As New List(Of String)
            Dim less1 As New List(Of String)
            Dim equal1 As New List(Of String)
            Dim greater1 As New List(Of String)

            For Each k As String In fields.Keys
                less0.Add(k)
            Next k
            equal0.AddRange({"AverageRadius", "expAmount"})
            If maxMages > 0 Then
                If mageSpellsMaxLevel < mageSpellsMinLevel Then msg &= vbNewLine & "mageSpellsMaxLevel < mageSpellsMinLevel"
                equal0.AddRange({"mageSpellsMaxLevel", "mageSpellsMinLevel", "mageSpellsCount"})
            End If
            If maxMercenaries > 0 Then
                If mercenariesMaxExpBar < mercenariesMinExpBar Then msg &= vbNewLine & "mercenariesMaxExpBar < mercenariesMinExpBar"
                equal0.AddRange({"mercenariesMaxExpBar", "mercenariesMinExpBar", "mercenariesCount"})
            End If
            If maxVendors > 0 Then
                If merchMaxItemCost < merchMinItemCost Then msg &= vbNewLine & "merchMaxItemCost < merchMinItemCost"
                equal0.AddRange({"merchMaxItemCost", "merchMinItemCost", "merchItemsCost"})
            End If
            less1.AddRange({"minStackToStackDist"})
            equal1.AddRange({"maxEccentricityDispersion", "maxRadiusDispersion"})
            greater1.AddRange({"maxEccentricityDispersion", "maxRadiusDispersion"})

            msg &= Map.CheckGenParameters(fields, less0, equal0, less1, equal1, greater1)
            If msg.Length > 0 Then msg = msg.Substring(1)
            Checked = True
            Return msg
        End Function
        ''' <summary>True, если проверка параметров запускалась</summary>
        Public Function isChecked() As Boolean
            Return Checked
        End Function

    End Structure
    Public Structure SettingsMap
        ''' <summary>Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 23)</summary>
        Dim xSize As Integer
        ''' <summary>Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 47)</summary>
        Dim ySize As Integer
        ''' <summary>Минимальное расстояние между проходами</summary>
        Dim minPassDist As Double
        ''' <summary>Минимальная ширина проходов</summary>
        Dim minPassWidth As Double
        ''' <summary>Количество рас</summary>
        Dim nRaces As Integer
        ''' <summary>Генератор будет располагать локации со столицами так, чтобы для каждой из локаций выполнялось следующиее условие:
        ''' R1*(1+T) >= R2, при этом R2 > R1, где R1 и R2 - расстояние до двух ближайших локаций со столицами</summary>
        Dim RaceLocsDistTolerance As Double
        ''' <summary>Расставлять ли стражей проходов между локациями</summary>
        Dim AddGuardsBetweenLocations As Boolean
        ''' <summary>Множитель силы стражей проходов между локациями</summary>
        Dim PassGuardsPowerMultiplicator As Double
        ''' <summary>Множитель силы стражей посещаемых объектов</summary>
        Dim ObjectGuardsPowerMultiplicator As Double
        ''' <summary>Отношение максимального опыта, получаемого за зачистку локации среднего размера, к минимальному.
        ''' Чем дальше локация от ближайшей столицы и чем ближе к центру, тем больше опыта за ее зачистку</summary>
        Dim LocExpRatio As Double
        ''' <summary>Множитель стоимости лута нейтралов</summary>
        Dim Wealth As Double
        ''' <summary>Количество воды на карте. 0 - без воды, 1 - очень много</summary>
        Dim WaterAmount As Double
        '''<summary>Максимальный уровень заклинаний в столице</summary>
        Dim SpellsMaxLevel As Integer
        ''' <summary>Количество дорого на карте. 0 - без дорог, 1 - максимальное количество</summary>
        Dim RoadsAmount As Double
        ''' <summary>Количество леса на карте. 0 - без леса, 1 - максимальное количество</summary>
        Dim ForestAmount As Double

        Private Checked As Boolean
        ''' <summary>Проверит корректность параметров. Вернет пустое сообщение, если все нормально</summary>
        Public Function Check() As String

            Dim fields As New Dictionary(Of String, String)

            fields.Add("xSize", xSize.ToString)
            fields.Add("ySize", ySize.ToString)
            fields.Add("minPassDist", minPassDist.ToString)
            fields.Add("minPassWidth", minPassWidth.ToString)
            fields.Add("nRaces", nRaces.ToString)
            fields.Add("RaceLocsDistTolerance", RaceLocsDistTolerance.ToString)
            'fields.Add("AddGuardsBetweenLocations", AddGuardsBetweenLocations.ToString)
            fields.Add("PassGuardsPowerMultiplicator", PassGuardsPowerMultiplicator.ToString)
            fields.Add("ObjectGuardsPowerMultiplicator", ObjectGuardsPowerMultiplicator.ToString)
            fields.Add("LocExpRatio", LocExpRatio.ToString)
            fields.Add("Wealth", Wealth.ToString)
            fields.Add("WaterAmount", WaterAmount.ToString)
            fields.Add("SpellsMaxLevel", SpellsMaxLevel.ToString)
            fields.Add("RoadsAmount", RoadsAmount.ToString)
            fields.Add("ForestAmount", ForestAmount.ToString)

            Dim msg As String = ""
            Dim less0 As New List(Of String)
            Dim equal0 As New List(Of String)
            Dim less1 As New List(Of String)
            Dim equal1 As New List(Of String)
            Dim greater1 As New List(Of String)

            For Each k As String In fields.Keys
                less0.Add(k)
            Next k
            equal0.AddRange({"SpellsMaxLevel", "ObjectGuardsPowerMultiplicator"})
            If AddGuardsBetweenLocations Then equal0.AddRange({"PassGuardsPowerMultiplicator"})
            less1.AddRange({"xSize", "ySize", "minPassDist", "minPassWidth", "nRaces", "LocExpRatio"})
            equal1.AddRange({"nRaces"})
            greater1.AddRange({"WaterAmount", "RoadsAmount", "ForestAmount"})

            msg &= Map.CheckGenParameters(fields, less0, equal0, less1, equal1, greater1)
            If msg.Length > 0 Then msg = msg.Substring(1)
            Checked = True
            Return msg
        End Function
        ''' <summary>True, если проверка параметров запускалась</summary>
        Public Function isChecked() As Boolean
            Return Checked
        End Function

    End Structure
    Private Shared Function CheckGenParameters(ByRef fields As Dictionary(Of String, String), ByRef less0 As List(Of String), ByRef equal0 As List(Of String), _
                                               ByRef less1 As List(Of String), ByRef equal1 As List(Of String), ByRef greater1 As List(Of String)) As String
        Dim result As String = ""
        For Each k As String In less0
            If CDbl(fields.Item(k)) < 0 Then result &= vbNewLine & k & " < 0"
        Next k
        For Each k As String In equal0
            If CDbl(fields.Item(k)) = 0 Then result &= vbNewLine & k & " = 0"
        Next k
        For Each k As String In less1
            If CDbl(fields.Item(k)) < 1 Then result &= vbNewLine & k & " < 1"
        Next k
        For Each k As String In equal1
            If CDbl(fields.Item(k)) = 1 Then result &= vbNewLine & k & " = 1"
        Next k
        For Each k As String In greater1
            If CDbl(fields.Item(k)) > 1 Then result &= vbNewLine & k & " > 1"
        Next k
        Return result
    End Function

    ''' <summary>Вернет True, если все нормально, иначе стоит перегенерировать</summary>
    Public Function TestMap() As String
        Dim imp As New ImpenetrableMeshGen
        If Not complited.LoationsCreation_Done Then
            Throw New Exception("Сначала нужно выполнить ImpenetrableMeshGen.SymmGen или ImpenetrableMeshGen.UnsymmGen")
        End If
        For x As Integer = 0 To xSize Step 1
            For y As Integer = 0 To ySize Step 1
                If board(x, y).isBorder And board(x, y).isAttended Then
                    Return "Warning: border and object are on the same place"
                ElseIf board(x, y).isBorder And board(x, y).isPass Then
                    Return "Warning: border and pass are on the same place"
                ElseIf board(x, y).isBorder And board(x, y).Penetrable Then
                    Return "Warning: border and penetrable cell are on the same place"
                ElseIf board(x, y).isAttended And board(x, y).Penetrable Then
                    Return "Warning: object and penetrable cell are on the same place"
                ElseIf board(x, y).isAttended And board(x, y).isPass Then
                    Return "Warning: object and pass are on the same place"
                End If
                If complited.StacksPlacing_Done And board(x, y).GuardLoc Then
                    If board(x, y).isBorder Then
                        Return "Warning: border and guard are on the same place"
                    ElseIf board(x, y).isAttended And board(x, y).objectID = 0 Then
                        Return "Warning: object and guard are on the same place"
                    ElseIf (board(x, y).objectID = 2 Or board(x, y).objectID = 7) And board(x, y).groupID < 1 Then
                        Return "Warning: group for internal guard for object is zero"
                    ElseIf board(x, y).objectID > 0 AndAlso imp.ActiveObjects(board(x, y).objectID).hasExternalGuard Then
                        Return "Warning: object with external guard has internal one"
                    End If
                End If
                If complited.StacksPlacing_Done Then
                    If Not board(x, y).GuardLoc And (board(x, y).objectID = 2 Or board(x, y).objectID = 7) Then
                        Return "Warning: internal guard for object is not set"
                    End If
                End If
                If complited.StacksPlacing_Done And board(x, y).PassGuardLoc Then
                    If board(x, y).isBorder Then
                        Return "Warning: border and pass guard are on the same place"
                    ElseIf board(x, y).isAttended Then
                        Return "Warning: object and pass guard are on the same place"
                    ElseIf board(x, y).GuardLoc Then
                        Return "Warning: common guard and pass guard are on the same place"
                    ElseIf board(x, y).groupID < 1 Then
                        Return "Warning: group for pass guard is zero"
                    End If
                End If
            Next y
        Next x
        If complited.StacksPlacing_Done Then
            complited.MeshTestII_Done = True
        Else
            complited.MeshTestI_Done = True
        End If
        Return ""
    End Function

End Class

Public Class StackLocationsGen

    Private genmap As New ImpenetrableMeshGen
    Private symm As New SymmetryOperations
    Private comm As New Common

    ''' <summary>Расставляет локации для отрядов на карту с подготовленную в InpenetrableMeshGen
    ''' Для руин и городов выставляет локации с параметрами отрядов там же, где и хранится objectID.
    ''' Если не получится, вернет False</summary>
    ''' <param name="m">Карта с расставленными объектами и границами между локациями</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону</param>
    ''' <param name="maxGenTime">Максимальное время на операцию расстановки стражей проходов между локациями.
    ''' Она обычно производится меньше чем за секунду, но бывает, что выполняется дольше минуты.
    ''' В этом случае быстрее перегенерировать карту</param>
    Public Function Gen(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                        ByVal settRaceLoc As Map.SettingsLoc, _
                        ByVal settCommLoc As Map.SettingsLoc, _
                        ByRef maxGenTime As Integer) As Boolean

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")
        If Not settRaceLoc.isChecked Then Throw New Exception("Check parameters via settRaceLoc.Check()")
        If Not settCommLoc.isChecked Then Throw New Exception("Check parameters via settCommLoc.Check()")

        If Not m.complited.LoationsCreation_Done Or Not m.complited.MeshTestI_Done Then
            Throw New Exception("Сначала нужно выполнить ImpenetrableMeshGen.SymmGen или " & _
                                "ImpenetrableMeshGen.UnsymmGen и протестировать результат с помощью m.TestMap")
        End If

        Dim tmpm As Map = m
        Dim GroupID As Integer
        Dim t0 As Integer = Environment.TickCount

        Call PlaceCommonStacks(tmpm, settMap, settRaceLoc, settCommLoc, GroupID)

        If settMap.AddGuardsBetweenLocations Then
            Dim term As New TerminationCondition(maxGenTime)
            Dim guards(UBound(m.Loc))()() As Point
            Parallel.For(0, m.Loc.Length,
             Sub(i As Integer)
                 If Not tmpm.Loc(i).IsObtainedBySymmery Then
                     If term.ExitFromLoops Then Exit Sub
                     guards(i) = PlasePassesGuards(tmpm, settMap, tmpm.Loc(i).ID, term)
                 End If
             End Sub)
            If term.ExitFromLoops Then Return False

            Dim gList() As Point = ConvertPointsArray(guards)

            Call RemoveExcessPassGuards(tmpm, settMap, gList)

            Call PlacePassGuards(tmpm, gList, GroupID, settMap)
        End If
        m = tmpm
        Dim t1 As Integer = Environment.TickCount
        Console.WriteLine("Stacks locations " & t1 - t0)
        m.complited.StacksPlacing_Done = True
        Return True
    End Function

    Private Sub PlaceCommonStacks(ByRef m As Map, ByVal settMap As Map.SettingsMap, _
                                  ByVal settRaceLoc As Map.SettingsLoc, _
                                  ByVal settCommLoc As Map.SettingsLoc, _
                                  ByRef GroupID As Integer)
        Dim tmpm As Map = m
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).objectID = 8 Then
                    Dim b As Location.Borders = genmap.ObjectBorders(m.board(x, y).objectID, x, y)
                    b.minX = Math.Max(b.minX, 0)
                    b.minY = Math.Max(b.minY, 0)
                    b.maxX = Math.Min(b.maxX, m.xSize)
                    b.maxY = Math.Min(b.maxY, m.ySize)
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If m.board(i, j).Penetrable And Not m.board(i, j).isPass Then
                                m.board(i, j).Penetrable = False
                            End If
                        Next i
                    Next j
                End If
                GroupID = Math.Max(GroupID, m.board(x, y).groupID)
            Next x
        Next y
        Dim posList(UBound(m.Loc)) As List(Of Point)
        Parallel.For(0, m.Loc.Length, _
         Sub(i As Integer)
             If Not tmpm.Loc(i).IsObtainedBySymmery Then
                 If i < settMap.nRaces Then
                     posList(i) = FillLocation(tmpm, settMap, settRaceLoc, tmpm.Loc(i).ID)
                 Else
                     posList(i) = FillLocation(tmpm, settMap, settCommLoc, tmpm.Loc(i).ID)
                 End If
             End If
         End Sub)

        For i As Integer = 0 To UBound(m.Loc) Step 1
            If Not m.Loc(i).IsObtainedBySymmery Then
                For Each p As Point In posList(i)
                    GroupID += 1
                    If m.symmID > -1 Then
                        Dim pp() As Point = symm.ApplySymm(p, settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(pp) Step 1
                            Dim tx As Integer = pp(k).X
                            Dim ty As Integer = pp(k).Y
                            m.board(pp(k).X, pp(k).Y).groupID = GroupID
                            m.board(pp(k).X, pp(k).Y).GuardLoc = True
                        Next k
                    Else
                        m.board(p.X, p.Y).groupID = GroupID
                        m.board(p.X, p.Y).GuardLoc = True
                    End If
                Next p
            End If
        Next i

        Dim protect(m.xSize, m.ySize) As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).objectID > 0 AndAlso genmap.ActiveObjects(m.board(x, y).objectID).hasExternalGuard Then
                    protect(x + genmap.ActiveObjects(m.board(x, y).objectID).Size, _
                            y + genmap.ActiveObjects(m.board(x, y).objectID).Size) = True
                End If
            Next x
        Next y

        Dim mDistR As Double = Math.Min(settRaceLoc.minStackToStackDist, 2)
        Dim mDistC As Double = Math.Min(settCommLoc.minStackToStackDist, 2)

        Dim tolerance As Integer = CInt(Math.Ceiling(Math.Max(mDistR, mDistC)))
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).GuardLoc Then
                    Dim d1, d2 As Double
                    If m.board(x, y).locID.Item(0) <= settMap.nRaces Then
                        d1 = mDistR
                    Else
                        d1 = mDistC
                    End If
                    Dim t As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, tolerance)
                    For j As Integer = t.minY To t.maxY Step 1
                        For i As Integer = t.minX To t.maxX Step 1
                            If m.board(i, j).GuardLoc And (Not x = i Or Not y = j) Then
                                Dim d As Double = New Point(x, y).SqDist(i, j)
                                If m.board(i, j).locID.Item(0) <= settMap.nRaces Then
                                    d2 = mDistR
                                Else
                                    d2 = mDistC
                                End If
                                If d < Math.Min(d1, d2) ^ 2 Then
                                    Dim p() As Point = New Point() {New Point(i, j), New Point(x, y)}
                                    For k As Integer = 0 To 1 Step 1
                                        If Not protect(p(k).X, p(k).Y) Then
                                            If m.symmID > -1 Then
                                                Dim pp() As Point = symm.ApplySymm(p(k), settMap.nRaces, m, 1)
                                                For Each item As Point In pp
                                                    m.board(item.X, item.Y).GuardLoc = False
                                                Next item
                                            Else
                                                m.board(p(k).X, p(k).Y).GuardLoc = False
                                            End If
                                            If k = 1 Then
                                                i = t.maxX
                                                j = t.maxY
                                            End If
                                        End If
                                    Next k
                                End If
                            End If
                        Next i
                    Next j
                End If
            Next x
        Next y

        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).objectID = 2 Or m.board(x, y).objectID = 7 Then
                    m.board(x, y).GuardLoc = True
                End If
            Next x
        Next y
        m = tmpm
    End Sub
    Private Function FillLocation(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                  ByRef settLoc As Map.SettingsLoc, ByRef LocID As Integer) As List(Of Point)

        Dim b As New Location.Borders With {.minX = Integer.MaxValue, .maxX = Integer.MinValue, _
                                            .miny = Integer.MaxValue, .maxy = Integer.MinValue}
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID.Item(0) = LocID Then
                    b.minX = Math.Min(b.minX, x)
                    b.minY = Math.Min(b.minY, y)
                    b.maxX = Math.Max(b.maxX, x)
                    b.maxY = Math.Max(b.maxY, y)
                End If
            Next x
        Next y

        Dim xSize As Integer = b.maxX - b.minX
        Dim ySize As Integer = b.maxY - b.minY
        Dim isPossiblePoint(xSize, ySize) As Boolean
        Dim LPos As New Point(b.minX, b.minY)

        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If m.board(x + LPos.X, y + LPos.Y).locID.Item(0) = LocID _
                And Not m.board(x + LPos.X, y + LPos.Y).Penetrable _
                And Not m.board(x + LPos.X, y + LPos.Y).isPass _
                And Not m.board(x + LPos.X, y + LPos.Y).isAttended _
                And Not m.board(x + LPos.X, y + LPos.Y).isBorder Then
                    isPossiblePoint(x, y) = True
                End If
            Next x
        Next y
        If m.symmID > -1 Then
            For y As Integer = 0 To ySize Step 1
                For x As Integer = 0 To xSize Step 1
                    If isPossiblePoint(x, y) Then
                        Dim p() As Point = symm.ApplySymm(New Point(x + b.minX, y + LPos.Y), settMap.nRaces, m, CInt(Math.Ceiling(settLoc.minStackToStackDist)))
                        If p.Length = settMap.nRaces Then
                            For k As Integer = 0 To UBound(p) Step 1
                                Dim tx As Integer = p(k).X - LPos.X
                                Dim ty As Integer = p(k).Y - LPos.Y
                                If tx >= 0 And ty >= 0 And tx <= xSize And ty <= ySize Then isPossiblePoint(tx, ty) = False
                            Next k
                            isPossiblePoint(x, y) = True
                        Else
                            isPossiblePoint(x, y) = False
                        End If
                    End If
                Next x
            Next y
        End If

        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isPossiblePoint(x, y) Then
                    Dim t As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, xSize, ySize, CInt(Math.Ceiling(settLoc.minStackToStackDist)))
                    For j As Integer = t.minY To t.maxY Step 1
                        For i As Integer = t.minX To t.maxX Step 1
                            If m.board(i + LPos.X, j + LPos.Y).GuardLoc _
                            AndAlso New Point(x, y).SqDist(i, j) < settLoc.minStackToStackDist * settLoc.minStackToStackDist Then
                                isPossiblePoint(x, y) = False
                            End If
                        Next i
                    Next j
                End If
            Next x
        Next y

        Dim StackPos(Math.Max(24, Environment.ProcessorCount * 6) - 1) As List(Of Point)
        Dim rms(UBound(StackPos)) As Double
        Dim r(), rcut, averR As Double
        Dim tryagain As Boolean
        Dim selected As Integer = 0
        r = Nothing
        For i As Integer = 0 To UBound(StackPos) Step 1
            StackPos(i) = PlaceStacks(isPossiblePoint, settLoc)
            If StackPos(i).Count > 1 Then
                tryagain = True
                rcut = 0
                Do While tryagain
                    tryagain = False
                    ReDim r(StackPos(i).Count - 1)
                    rcut += settLoc.minStackToStackDist
                    For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                        For q As Integer = 0 To StackPos(i).Count - 1 Step 1
                            If Not p = q Then
                                Dim d As Integer = StackPos(i)(p).SqDist(StackPos(i)(q))
                                If d <= rcut * rcut Then r(p) += Math.Sqrt(d)
                            End If
                        Next q
                    Next p
                    For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                        If r(p) = 0 Then
                            tryagain = True
                            Exit For
                        End If
                    Next p
                Loop
                averR = 0
                For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                    averR += r(p)
                Next p
                averR /= (StackPos(i).Count - 1)
                For p As Integer = 0 To StackPos(i).Count - 1 Step 1
                    rms(i) += (r(p) - averR) ^ 2
                Next p
                rms(i) = Math.Sqrt(rms(i) / (StackPos(i).Count * (StackPos(i).Count - 1))) / rcut
            ElseIf StackPos(i).Count = 1 Then
                rms(i) = 1
            Else
                rms(i) = Double.MaxValue / 10
            End If
            If rms(i) < rms(selected) Then selected = i
        Next i
        Dim res As New List(Of Point)
        For Each p As Point In StackPos(selected)
            res.Add(New Point(p.X + LPos.X, p.Y + LPos.Y))
        Next p
        Return res
    End Function
    Private Function PlaceStacks(ByRef isPossiblePoint(,) As Boolean, ByRef settLoc As Map.SettingsLoc) As List(Of Point)
        Dim tolerance As Integer = CInt(Math.Ceiling(settLoc.minStackToStackDist))
        Dim minDistSq As Integer = CInt(Math.Ceiling(settLoc.minStackToStackDist * settLoc.minStackToStackDist))
        Dim xSize As Integer = UBound(isPossiblePoint, 1)
        Dim ySize As Integer = UBound(isPossiblePoint, 2)
        Dim PossiblePoints(isPossiblePoint.Length - 1) As Point
        Dim PosPID(xSize, ySize) As Integer
        Dim IDs As New List(Of Integer)
        Dim n As Integer = -1
        For y As Integer = 0 To ySize Step 1
            For x As Integer = 0 To xSize Step 1
                If isPossiblePoint(x, y) Then
                    n += 1
                    PossiblePoints(n) = New Point(x, y)
                    PosPID(x, y) = n
                    IDs.Add(n)
                Else
                    PosPID(x, y) = -1
                End If
            Next x
        Next y
        ReDim Preserve PossiblePoints(n)
        Dim output As New List(Of Point)
        Do While IDs.Count > 0
            Dim r As Integer = comm.RandomSelection(IDs, False)
            output.Add(PossiblePoints(r))
            IDs.Remove(r)
            PosPID(PossiblePoints(r).X, PossiblePoints(r).Y) = -1
            Dim t As Location.Borders = ImpenetrableMeshGen.NearestXY(PossiblePoints(r).X, PossiblePoints(r).Y, xSize, ySize, tolerance)
            For j As Integer = t.minY To t.maxY Step 1
                For i As Integer = t.minX To t.maxX Step 1
                    If PosPID(i, j) > -1 AndAlso PossiblePoints(r).SqDist(i, j) < minDistSq Then
                        IDs.Remove(PosPID(i, j))
                        PosPID(i, j) = -1
                    End If
                Next i
            Next j
        Loop
        Return output
    End Function

    Private Function PlasePassesGuards(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                       ByRef LocID As Integer, ByRef term As TerminationCondition) As Point()()

        Dim passes, gag As New List(Of String)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID.Item(0) = LocID And m.board(x, y).isPass Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 1)
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If m.board(i, j).locID.Item(0) > LocID And m.board(i, j).isPass Then
                                Dim s As String = i & "_" & j
                                If Not passes.Contains(s) Then passes.Add(s)
                                s = x & "_" & y
                                If Not passes.Contains(s) Then passes.Add(s)
                            ElseIf m.board(i, j).locID.Item(0) < LocID And m.board(i, j).isPass Then
                                Dim s As String = i & "_" & j
                                If Not gag.Contains(s) Then gag.Add(s)
                                s = x & "_" & y
                                If Not gag.Contains(s) Then gag.Add(s)
                            End If
                        Next i
                    Next j
                End If
            Next x
        Next y
        'For Each s As String In passes
        '    If gag.Contains(s) Then gag.Remove(s)
        'Next s
        Dim FreeInClosedState(m.xSize, m.ySize) As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If Not m.board(x, y).isBorder And Not m.board(x, y).isAttended Then
                    Dim s As String = x & "_" & y
                    If Not gag.Contains(s) AndAlso Not passes.Contains(s) Then FreeInClosedState(x, y) = True
                End If
            Next x
        Next y
        gag = Nothing
        For Each s As String In passes
            Dim splited() As String = s.Split(CChar("_"))
            Dim p As New Point(CInt(splited(0)), CInt(splited(1)))
            Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(p, m, 1)
            For j As Integer = b.minY To b.maxY Step 1
                For i As Integer = b.minX To b.maxX Step 1
                    FreeInClosedState(i, j) = False
                Next i
            Next j
        Next s


        Dim free(m.xSize, m.ySize), connected()(,) As Boolean
        For Each k As String In passes
            Dim s() As String = k.Split(CChar("_"))
            free(CInt(s(0)), CInt(s(1))) = True
        Next k
        connected = GetConnected(free)
        If Not IsNothing(connected) Then
            Dim T As TerminationCondition
            Dim NConnClosed As Integer = NConnected(FreeInClosedState, m) 'GetConnected(FreeInClosedState).Length
            Dim out(UBound(connected))() As Point
            For j As Integer = 0 To UBound(connected) Step 1
                T = New TerminationCondition(term.maxTime)
                'нужно передавать tfree, а освобождать не только connected, но и те, что рядом с ним
                out(j) = HandlePath(m, connected(j), FreeInClosedState, NConnClosed, T)
                If T.ExitFromLoops Then
                    term.ExitFromLoops = True
                    Return Nothing
                End If
            Next j
            Return out
        End If
        Return Nothing
    End Function
    Private Function HandlePath(ByRef m As Map, ByRef path(,) As Boolean, _
                                ByRef FreeInClosedState(,) As Boolean, ByRef NConnClosed As Integer, _
                                ByRef term As TerminationCondition) As Point()
        Dim opened(,) As Boolean = CType(FreeInClosedState.Clone, Boolean(,))
        Dim n As Integer = -1
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If path(x, y) Then
                    n += 1
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 1)
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If Not m.board(i, j).isBorder And Not m.board(i, j).isAttended Then opened(i, j) = True
                        Next i
                    Next j
                End If
            Next x
        Next y
        Dim pointsList(n) As Point
        Dim IDs As New List(Of Integer)
        n = -1
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If path(x, y) Then
                    n += 1
                    pointsList(n) = New Point(x, y)
                    IDs.Add(n)
                End If
            Next x
        Next y

        Dim output(0) As Point
        For k As Integer = 0 To UBound(pointsList) Step 1
            term.CheckTime()
            If term.ExitFromLoops Then Return Nothing
            ReDim output(k)
            Call PlaceGuardLoc(-1, pointsList, IDs, -1, 4, opened, NConnClosed, output, m, term)
            If Not IsNothing(output(0)) Then Exit For
        Next k
        If IsNothing(output(0)) Then
            For k As Integer = 0 To UBound(pointsList) Step 1
                term.CheckTime()
                If term.ExitFromLoops Then Return Nothing
                ReDim output(k)
                Call PlaceGuardLoc(-1, pointsList, IDs, -1, 0, opened, NConnClosed, output, m, term)
                If Not IsNothing(output(0)) Then Exit For
            Next k
        End If
        Return output
    End Function
    Private Function GetConnected(ByRef free(,) As Boolean) As Boolean()(,)
        Dim connected()(,) As Boolean = Nothing
        Dim p As Point = New Point(-1, -1)
        Do While Not IsNothing(p)
            p = genmap.FindDisconnected(free, connected)
            If Not IsNothing(p) Then
                If IsNothing(connected) Then
                    ReDim connected(0)
                Else
                    ReDim Preserve connected(connected.Length)
                End If
                connected(UBound(connected)) = genmap.FindConnected(free, p)
            End If
        Loop
        Return connected
    End Function
    Private Sub PlaceGuardLoc(ByRef n As Integer, ByRef pointsList() As Point, ByRef IDs As List(Of Integer), _
                              ByRef selected As Integer, ByRef minSqDist As Integer, ByRef opened(,) As Boolean, _
                              ByRef NConnClosed As Integer, ByRef output() As Point, ByRef m As Map, _
                              ByRef term As TerminationCondition)
        term.CheckTime()
        If term.ExitFromLoops Then
            output(n) = Nothing
            Exit Sub
        End If

        If n > -1 Then output(n) = pointsList(selected)
        If n < UBound(output) Then
            Dim idsBak As New List(Of Integer)
            For Each i As Integer In IDs
                If Not i = selected Then
                    If minSqDist = 0 OrElse selected < 0 OrElse pointsList(i).SqDist(pointsList(selected)) >= minSqDist Then
                        idsBak.Add(i)
                    End If
                End If
            Next i
            Do While idsBak.Count > 0
                Dim sel As Integer = comm.RandomSelection(idsBak, False)
                Call PlaceGuardLoc(n + 1, pointsList, idsBak, sel, minSqDist, opened, NConnClosed, output, m, term)
                If IsNothing(output(n + 1)) Then
                    idsBak.Remove(sel)
                Else
                    idsBak.Clear()
                End If
            Loop
            If IsNothing(output(n + 1)) And n > -1 Then output(n) = Nothing
        Else
            Dim tp(,) As Boolean = CType(opened.Clone, Boolean(,))
            For Each p As Point In output
                Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(p.X, p.Y, UBound(tp, 1), UBound(tp, 2), 1)
                For j As Integer = b.minY To b.maxY Step 1
                    For i As Integer = b.minX To b.maxX Step 1
                        tp(i, j) = False
                    Next i
                Next j
            Next p
            Dim r As Integer = NConnected(tp, m) 'GetConnected(tp).Length
            If NConnClosed > r Then output(n) = Nothing
        End If
    End Sub
    Private Function NConnected(ByRef free(,) As Boolean, ByRef m As Map) As Integer
        Dim conn()(,) As Boolean = GetConnected(free)
        Dim n As Integer = 0
        For i As Integer = 0 To UBound(conn) Step 1
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If conn(i)(x, y) And Not m.board(x, y).isPass Then
                        n += 1
                        x = m.xSize
                        y = m.ySize
                    End If
                Next x
            Next y
        Next i
        Return n
    End Function

    Private Function ConvertPointsArray(ByRef guards()()() As Point) As Point()
        If IsNothing(guards) Then Return Nothing
        Dim n As Integer = -1
        For i As Integer = 0 To UBound(guards) Step 1
            If Not IsNothing(guards(i)) Then
                For j As Integer = 0 To UBound(guards(i)) Step 1
                    If Not IsNothing(guards(i)(j)) Then n += guards(i)(j).Length
                Next j
            End If
        Next i
        If n = -1 Then Return Nothing
        Dim res(n) As Point
        n = -1
        For i As Integer = 0 To UBound(guards) Step 1
            If Not IsNothing(guards(i)) Then
                For j As Integer = 0 To UBound(guards(i)) Step 1
                    If Not IsNothing(guards(i)(j)) Then
                        For k As Integer = 0 To UBound(guards(i)(j)) Step 1
                            n += 1
                            res(n) = guards(i)(j)(k)
                        Next k
                    End If
                Next j
            End If
        Next i
        Return res
    End Function
    Private Sub RemoveExcessPassGuards(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                                       ByRef guards() As Point)
        If IsNothing(guards) Then Exit Sub
        Dim free(m.xSize, m.ySize) As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If Not m.board(x, y).isBorder And Not m.board(x, y).isAttended Then free(x, y) = True
            Next x
        Next y
        Dim n As Integer
        Dim excluded As New List(Of Integer)
        Dim nLonsInFullyClosedState As Integer = CalcNDisconnectedLocs(free, m, settMap, guards, excluded)

        For i As Integer = 0 To UBound(guards) Step 1
            excluded.Add(i)
            n = CalcNDisconnectedLocs(free, m, settMap, guards, excluded)
            If nLonsInFullyClosedState > n Then excluded.Remove(i)
        Next i
        If guards.Length = excluded.Count Then
            guards = Nothing
            Exit Sub
        End If
        Dim res(UBound(guards) - excluded.Count) As Point
        n = -1
        For i As Integer = 0 To UBound(guards) Step 1
            If Not excluded.Contains(i) Then
                n += 1
                res(n) = New Point(guards(i).X, guards(i).Y)
            End If
        Next i
        guards = res
    End Sub
    Private Function CalcNDisconnectedLocs(ByRef free(,) As Boolean, ByRef m As Map, _
                                           ByRef settMap As Map.SettingsMap, _
                                           ByRef guards() As Point, ByRef excluded As List(Of Integer)) As Integer
        Dim freeInClosedState(,) As Boolean = CType(free.Clone, Boolean(,))

        For id As Integer = 0 To UBound(guards) Step 1
            If Not excluded.Contains(id) Then
                Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(guards(id), m, 1)
                For x As Integer = b.minX To b.maxX Step 1
                    For y As Integer = b.minY To b.maxY Step 1
                        If m.symmID > -1 Then
                            Dim pp() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 0)
                            For Each item As Point In pp
                                freeInClosedState(item.X, item.Y) = False
                            Next item
                        Else
                            freeInClosedState(x, y) = False
                        End If
                    Next y
                Next x
            End If
        Next id

        Return NConnected(freeInClosedState, m)
    End Function

    Private Sub PlacePassGuards(ByRef m As Map, ByRef guards() As Point, ByRef GroupID As Integer, ByRef settMap As Map.SettingsMap)
        If IsNothing(guards) Then Exit Sub
        For Each p As Point In guards
            GroupID += 1
            If m.symmID > -1 Then
                Dim pp() As Point = symm.ApplySymm(p, settMap.nRaces, m, 1)
                For Each item As Point In pp
                    m.board(item.X, item.Y).groupID = GroupID
                    m.board(item.X, item.Y).PassGuardLoc = True
                Next item
            Else
                m.board(p.X, p.Y).groupID = GroupID
                m.board(p.X, p.Y).PassGuardLoc = True
            End If
        Next p
    End Sub
End Class

Public Class Point
    ''' <summary>Координата по X</summary>
    Public X As Integer
    ''' <summary>Координата по Y</summary>
    Public Y As Integer
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Sub New(ByVal X As Integer, ByVal Y As Integer)
        Me.X = X
        Me.Y = Y
    End Sub

    ''' <summary>Возвращает квадрат расстояния до точки</summary>
    ''' <param name="p">Координаты точки</param>
    Public Function SqDist(ByRef p As Point) As Integer
        Return SqDist(p.X, p.Y)
    End Function
    ''' <summary>Возвращает квадрат расстояния до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function SqDist(ByRef x As Integer, ByRef y As Integer) As Integer
        Dim dx As Integer = Me.X - x
        Dim dy As Integer = Me.Y - y
        Return dx * dx + dy * dy
    End Function
    ''' <summary>Возвращает квадрат расстояния до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function SqDist(ByRef x As Double, ByRef y As Double) As Double
        Dim dx As Double = CDbl(Me.X) - x
        Dim dy As Double = CDbl(Me.Y) - y
        Return dx * dx + dy * dy
    End Function
    ''' <summary>Возвращает расстояние до точки</summary>
    ''' <param name="p">Координаты точки</param>
    Public Function Dist(ByRef p As Point) As Double
        Return Dist(p.X, p.Y)
    End Function
    ''' <summary>Возвращает расстояние до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function Dist(ByRef x As Integer, ByRef y As Integer) As Double
        Return Math.Sqrt(SqDist(x, y))
    End Function
    ''' <summary>Возвращает расстояние до точки</summary>
    ''' <param name="x">Координата по X</param>
    ''' <param name="y">Координата по Y</param>
    Public Function Dist(ByRef x As Double, ByRef y As Double) As Double
        Return Math.Sqrt(SqDist(x, y))
    End Function

End Class

Public Class WaterGen

    Public ReadOnly minLocationRadiusAtAll As Double
    Public Sub New()
        minLocationRadiusAtAll = 2
    End Sub

    Private rndgen As New RndValueGen
    Private comm As New Common
    Private symm As New SymmetryOperations
    Private imp As New ImpenetrableMeshGen

    '''<summary>Сгенерирует озера на карте</summary>
    ''' <param name="m">Карта с сгенерированной силой отрядов</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap)

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")
        
        If Not m.complited.StacksDesiredStatsGen_Done Then
            Throw New Exception("Сначала нужно выполнить StackPowerGen.Gen")
        End If

        Dim t0 As Integer = Environment.TickCount
        Dim HaveToBeGround(m.xSize, m.ySize) As Boolean
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If m.board(i, j).isAttended Or m.board(i, j).Penetrable Then HaveToBeGround(i, j) = True
                If m.board(i, j).objectID = 8 Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            HaveToBeGround(x, y) = True
                        Next y
                    Next x
                End If
            Next j
        Next i
        If m.symmID > -1 Then
            For i As Integer = 0 To m.xSize Step 1
                For j As Integer = 0 To m.ySize Step 1
                    If HaveToBeGround(i, j) Then
                        Dim pp() As Point = symm.ApplySymm(New Point(i, j), settMap.nRaces, m, 1)
                        For k As Integer = 0 To UBound(pp) Step 1
                            Dim tx As Integer = pp(k).X
                            Dim ty As Integer = pp(k).Y
                            HaveToBeGround(tx, ty) = True
                        Next k
                    End If
                Next j
            Next i
        End If

        For i As Integer = 0 To m.Loc.Length - 1 Step 1
            If Not m.Loc(i).IsObtainedBySymmery Then Call PlaceWater(m, m.Loc(i), settMap, HaveToBeGround)
        Next i
        Call Extend(m, settMap, HaveToBeGround)
        m.complited.WaterCreation_Done = True
        Console.WriteLine("Water gen " & Environment.TickCount - t0)
    End Sub

    Private Sub PlaceWater(ByRef m As Map, ByRef loc As Location, ByRef settMap As Map.SettingsMap, ByRef HaveToBeGround(,) As Boolean)

        Dim lake As Location
        Dim WaterLocSettings As New Map.SettingsLoc With {.AverageRadius = CInt(0.3 * Math.Sqrt(loc.gASize * loc.gBSize) * settMap.WaterAmount), _
                                                          .maxRadiusDispersion = 0.25, _
                                                          .maxEccentricityDispersion = 0.35}
        Dim freeCell(,) As Boolean = Nothing
        Dim WaterAmount As Integer = WaterAmountCalc(m, loc, settMap, freeCell, HaveToBeGround)
        Dim n As Integer = -1
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If freeCell(i, j) Then n += 1
            Next j
        Next i
        Dim points(n) As Point
        Dim AllIds As New List(Of Integer)
        n = -1
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If freeCell(i, j) Then
                    n += 1
                    points(n) = New Point(i, j)
                    AllIds.Add(n)
                End If
            Next j
        Next i

        Do While WaterAmount > minLocationRadiusAtAll * minLocationRadiusAtAll
            lake = Location.GenLocSize(WaterLocSettings, 0, rndgen, minLocationRadiusAtAll)
            Dim selected As Integer = SelectLakePlace(AllIds, points, m, 0.5 * WaterLocSettings.AverageRadius)
            If selected < 0 Then Exit Do
            Call PlaceLake(AllIds, points, m, settMap, selected, freeCell, lake, WaterAmount, HaveToBeGround)
        Loop

        Call MakeRuinsWatered(m, loc, settMap)

    End Sub
    Private Function WaterAmountCalc(ByRef m As Map, ByRef loc As Location, ByRef settMap As Map.SettingsMap, _
                                     ByRef freeCell(,) As Boolean, ByRef HaveToBeGround(,) As Boolean) As Integer
        ReDim freeCell(m.xSize, m.ySize)
        Dim WaterAmount, Watered As Integer
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If m.board(i, j).locID(0) = loc.ID And Not HaveToBeGround(i, j) Then
                    If m.board(i, j).isBorder Then
                        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                        For x As Integer = b.minX To b.maxX Step 1
                            For y As Integer = b.minY To b.maxY Step 1
                                If Not m.board(x, y).isBorder And Not HaveToBeGround(x, y) Then
                                    WaterAmount += 1
                                    If m.board(i, j).isWater Then
                                        Watered += 1
                                    Else
                                        freeCell(i, j) = True
                                    End If
                                    x = b.maxX
                                    y = b.maxY
                                End If
                            Next y
                        Next x
                    Else
                        WaterAmount += 1
                        If m.board(i, j).isWater Then
                            Watered += 1
                        Else
                            freeCell(i, j) = True
                        End If
                    End If
                End If
            Next j
        Next i
        WaterAmount = CInt(Math.Floor(WaterAmount * Math.Min(settMap.WaterAmount, 1))) - Watered
        Return WaterAmount
    End Function
    Private Function SelectLakePlace(ByRef AllIds As List(Of Integer), ByRef points() As Point, ByRef m As Map, ByRef lakeDist As Double) As Integer
        Dim IDs As New List(Of Integer)
        Dim add As Boolean
        Dim b As Location.Borders
        For p As Integer = 0 To 1 Step 1
            For Each i As Integer In AllIds
                add = True
                b = ImpenetrableMeshGen.NearestXY(points(i), m, CInt(lakeDist))
                For x As Integer = b.minX To b.maxX Step 1
                    For y As Integer = b.minY To b.maxY Step 1
                        If m.board(x, y).isWater AndAlso (p = 1 OrElse points(i).SqDist(x, y) < lakeDist * lakeDist) Then
                            add = False
                            x = b.maxX
                            y = b.maxY
                        End If
                    Next y
                Next x
                If add Then IDs.Add(i)
            Next i
            If IDs.Count > 0 Then Exit For
        Next p
        If IDs.Count = 0 Then Return -1
        Return comm.RandomSelection(IDs, True)
    End Function
    Private Sub PlaceLake(ByRef AllIds As List(Of Integer), ByRef points() As Point, ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                          ByRef selected As Integer, ByRef freeCell(,) As Boolean, ByRef lake As Location, ByRef WaterAmount As Integer, ByRef HaveToBeGround(,) As Boolean)
        'размещаем эллиптическое озеро
        AllIds.Remove(selected)
        lake.pos = points(selected)
        Dim waterTiles, coastalTile, internalLakeTile As New List(Of Point)
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If Not HaveToBeGround(i, j) AndAlso lake.IsInside(i, j) Then
                    waterTiles.Add(New Point(i, j))
                    Call SetWaterCellSymm(i, j, m, freeCell, WaterAmount, settMap)
                End If
            Next j
        Next i

        'делаем берег неровным
        For L As Integer = 0 To 1 Step 1
            For Each p As Point In waterTiles
                If m.board(p.X, p.Y).isWater Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(p, m, 1)
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            If Not m.board(x, y).isWater Then
                                coastalTile.Add(p)
                                x = b.maxX
                                y = b.maxY
                            End If
                        Next y
                    Next x
                End If
            Next p
            For Each p As Point In coastalTile
                If rndgen.PRand(0, 1) < 0.25 - 0.1 * CDbl(L) Then
                    Call SetGroundCellSymm(p.X, p.Y, m, settMap)
                End If
            Next p
            coastalTile.Clear()
        Next L

        'размещем острова
        Dim minIslandR As Integer = 2
        Dim maxIslandR As Integer = 3
        Dim threshold As Double = 1 - (1 - settMap.WaterAmount) / ((0.5 * (minIslandR + maxIslandR)) ^ 2)
        Dim dD As Double = 0.05
        For Each p As Point In waterTiles
            If m.board(p.X, p.Y).isWater Then
                Dim maybe As Boolean = True
                Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(p, m, 1)
                For x As Integer = b.minX To b.maxX Step 1
                    For y As Integer = b.minY To b.maxY Step 1
                        If Not m.board(x, y).isWater Then
                            maybe = False
                            x = b.maxX
                            y = b.maxY
                        End If
                    Next y
                Next x
                If maybe AndAlso rndgen.PRand(0, 1) > threshold Then
                    Dim R As Double = rndgen.PRand(CDbl(minIslandR), CDbl(maxIslandR))
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            Dim d As Double = p.Dist(x, y)
                            If m.board(x, y).isWater AndAlso d < R Then
                                If rndgen.PRand(0, R + 2 * dD) > d + dD Then
                                    Call SetGroundCellSymm(x, y, m, settMap)
                                End If
                            End If
                        Next y
                    Next x
                End If
            End If
        Next p

    End Sub
    Private Sub SetWaterCellSymm(ByRef i As Integer, ByRef j As Integer, ByRef m As Map, ByRef freeCell(,) As Boolean, _
                                 ByRef WaterAmount As Integer, ByRef settMap As Map.SettingsMap)
        If m.symmID > -1 Then
            Dim pp() As Point = symm.ApplySymm(New Point(i, j), settMap.nRaces, m, 1)
            For k As Integer = 0 To UBound(pp) Step 1
                Dim tx As Integer = pp(k).X
                Dim ty As Integer = pp(k).Y
                Call SetWaterCell(tx, ty, m, freeCell, WaterAmount)
            Next k
        Else
            Call SetWaterCell(i, j, m, freeCell, WaterAmount)
        End If
    End Sub
    Private Sub SetWaterCell(ByRef i As Integer, ByRef j As Integer, ByRef m As Map, ByRef freeCell(,) As Boolean, ByRef WaterAmount As Integer)
        If Not IsNothing(freeCell) AndAlso freeCell(i, j) Then
            freeCell(i, j) = False
            WaterAmount -= 1
        End If
        m.board(i, j).isWater = True
    End Sub
    Private Sub SetGroundCellSymm(ByRef i As Integer, ByRef j As Integer, ByRef m As Map, ByRef settMap As Map.SettingsMap)
        If m.symmID > -1 Then
            Dim pp() As Point = symm.ApplySymm(New Point(i, j), settMap.nRaces, m, 1)
            For k As Integer = 0 To UBound(pp) Step 1
                Dim tx As Integer = pp(k).X
                Dim ty As Integer = pp(k).Y
                m.board(tx, ty).isWater = False
            Next k
        Else
            m.board(i, j).isWater = False
        End If
    End Sub

    Private Sub MakeRuinsWatered(ByRef m As Map, ByRef loc As Location, ByRef settMap As Map.SettingsMap)
        Dim chance As Double = 0.1 + settMap.WaterAmount * 0.4
        Dim makeWatered As Boolean
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If m.board(i, j).locID(0) = loc.ID And m.board(i, j).objectID = 7 Then
                    makeWatered = (rndgen.PRand(0, 1) < chance)

                    Dim d As Integer = 1
                    Dim x1 As Integer = Math.Max(i - d, 0)
                    Dim x2 As Integer = Math.Min(i + imp.ActiveObjects(m.board(i, j).objectID).Size + d - 1, m.xSize)
                    Dim y1 As Integer = Math.Max(j - d, 0)
                    Dim y2 As Integer = Math.Min(j + imp.ActiveObjects(m.board(i, j).objectID).Size + d - 1, m.ySize)
                    For x As Integer = x1 To x2 Step 1
                        For y As Integer = y1 To y2 Step 1
                            If makeWatered Then
                                Call SetWaterCellSymm(x, y, m, Nothing, Nothing, settMap)
                            Else
                                Call SetGroundCellSymm(x, y, m, settMap)
                            End If
                        Next y
                    Next x

                    x1 = Math.Max(x1 - 1, 0)
                    x2 = Math.Min(x2 + 1, m.xSize)
                    y1 = Math.Max(y1 - 1, 0)
                    y2 = Math.Min(y2 + 1, m.ySize)
                    For x As Integer = x1 To x2 Step 1
                        For y As Integer = y1 To y2 Step 1
                            If (x = x1 Or x = x2 Or y = y1 Or y = y2) And Not ((x = x1 Or x = x2) And (y = y1 Or y = y2)) And Not m.board(x, y).isAttended Then
                                If rndgen.PRand(0, 1) < 0.45 Then
                                    If makeWatered Then
                                        Call SetWaterCellSymm(x, y, m, Nothing, Nothing, settMap)
                                    Else
                                        Call SetGroundCellSymm(x, y, m, settMap)
                                    End If
                                End If
                            End If
                        Next y
                    Next x
                End If
            Next j
        Next i
    End Sub

    Private Sub Extend(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef HaveToBeGround(,) As Boolean)
        Dim n As Integer
        Dim var As New List(Of Point)
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If m.board(i, j).isWater Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                    n = 0
                    var.Clear()
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            If x = i Or y = j Then
                                If m.board(x, y).isWater Then
                                    n += 1
                                Else
                                    If (Not x = i Or Not y = j) And Not HaveToBeGround(x, y) Then var.Add(New Point(x, y))
                                End If
                            End If
                        Next y
                    Next x
                    If n < 2 And var.Count > 0 Then
                        n = rndgen.RndPos(var.Count, True) - 1
                        Dim p As Point = var.Item(n)
                        Call SetWaterCellSymm(p.X, p.Y, m, Nothing, Nothing, settMap)
                    End If
                End If
            Next j
        Next i
        For i As Integer = 0 To m.xSize Step 1
            For j As Integer = 0 To m.ySize Step 1
                If m.board(i, j).isWater Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                    n = 0
                    For x As Integer = b.minX To b.maxX Step 1
                        For y As Integer = b.minY To b.maxY Step 1
                            If x = i Or y = j Then
                                If m.board(x, y).isWater Then n += 1
                            End If
                        Next y
                    Next x
                    If n < 2 Then Call SetGroundCellSymm(i, j, m, settMap)
                End If
            Next j
        Next i
    End Sub

End Class

Class MapObject
    Inherits DecorationPlacingPropertiesFields
    ''' <summary>Ширина объекта</summary>
    Protected Friend xSize As Integer
    ''' <summary>Высота объекта</summary>
    Protected Friend ySize As Integer
    ''' <summary>Имя объекта</summary>
    Protected Friend name As String
End Class
Class Plateau
    Inherits MapObject
    ''' <summary>Как скреплять объекты</summary>
    Protected Friend connectors()() As Point
    ''' <summary>Эти точки должны быть на границе карты</summary>
    Protected Friend border As Point
    ''' <summary>Является ли водопадом</summary>
    Protected Friend isWaterfall As Boolean
End Class
Public Class ImpenetrableObjects

    Private symm As New SymmetryOperations
    Private comm As New Common
    Private objects, mountains, ruins, mages, merchants, mercenaries, trainers As MapObject()
    Private plateau() As Plateau
    Private maxPlateauSize As Integer
    Private maxChainLen As Integer = 7
    Private raceSpells() As AllDataStructues.Spell
    Private raceIdToString As New Dictionary(Of Integer, String)

    Private Structure PlateauPlacingResult
        Dim obj() As Plateau
        Dim pos() As Point
        Dim n As Integer
    End Structure

    ''' <param name="ObjectsSize">Размеры всех объектов. Ключи - ID объектов</param>
    ''' <param name="ExcludeLists">Файлы со списками исключенных объектов. Записи в них могут повторяться. 
    ''' Допускается передача неинициализитрованного массива.
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    ''' <param name="CustomBuildingRace">Файлы со списками рас объектов. Записи в них могут повторяться, но записи с повторяющимся ID будут перезаписываться. 
    ''' Допускается передача неинициализитрованного массива (тогда прочтем дефолтный файл).
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    ''' <param name="PlateauConstructionDescription">Файлы с описаниями.
    ''' Допускается передача неинициализитрованного массива (будет прочтен дефолтный).
    ''' Для чтения из дефолтного листа в массив нужно добавить строчку %default% (наличие этого ключевого в файле запустит чтение дефолтного файла)</param>
    ''' <param name="spells">Все заклинания в игре</param>
    Public Sub New(ByRef ObjectsSize As Dictionary(Of String, Size), ByRef ExcludeLists() As String, ByRef CustomBuildingRace() As String, _
                   ByRef PlateauConstructionDescription() As String, ByRef spells() As AllDataStructues.Spell)
        Call comm.ReadExcludedObjectsList(ExcludeLists)
        Call comm.ReadCustomBuildingRace(CustomBuildingRace)
        Call comm.ReadPlateauConstructionDescription(PlateauConstructionDescription)

        raceSpells = spells

        Dim objType() As String = New String() {My.Resources.objKeyMage, My.Resources.objKeyMercenaries, My.Resources.objKeyMerchant, _
                                                My.Resources.objKeyMountain, My.Resources.objKeyRuin, My.Resources.objKeyTrainer, ""}
        Dim objList(UBound(objType))() As MapObject
        For Each k As String In ObjectsSize.Keys
            If Not comm.excludedObjects.Contains(k.ToUpper) AndAlso comm.objectRace.ContainsKey(k.ToUpper) Then
                If comm.objectRace.Item(k.ToUpper).race.Count > 0 _
                And (comm.objectRace.Item(k.ToUpper).ground Or comm.objectRace.Item(k.ToUpper).water) Then

                    Dim g As MapObject = New MapObject With {.xSize = ObjectsSize.Item(k).Width, _
                                                             .ySize = ObjectsSize.Item(k).Height, _
                                                             .ground = comm.objectRace.Item(k.ToUpper).ground, _
                                                             .water = comm.objectRace.Item(k.ToUpper).water, _
                                                             .race = comm.objectRace.Item(k.ToUpper).race, _
                                                             .name = k.ToUpper}
                    If comm.PlateauConstruction.ContainsKey(k.ToUpper) Then
                        Dim isWaterfall As Boolean = False
                        Dim connectors()() As Point = Nothing
                        Dim boundary As Point = Nothing
                        Dim t() As String = comm.PlateauConstruction.Item(k.ToUpper).ToUpper.Replace(vbTab, " ").Split(CChar(" "))
                        Dim dx As Integer = 3 - g.xSize
                        Dim dy As Integer = 3 - g.ySize
                        For i As Integer = 0 To UBound(t) Step 1
                            If t(i) = "W" Then
                                isWaterfall = True
                            ElseIf t(i) = "U" Or t(i) = "D" Or t(i) = "L" Or t(i) = "R" Then
                                Dim p1 As Point = Nothing
                                Dim p2 As Point = Nothing
                                If IsNothing(connectors) Then
                                    ReDim connectors(0)
                                Else
                                    ReDim Preserve connectors(connectors.Length)
                                End If
                                If t(i) = "U" Then
                                    p1 = New Point(1, -1)
                                    p2 = New Point(1, 0)
                                ElseIf t(i) = "D" Then
                                    p1 = New Point(1, 2 - dy)
                                    p2 = New Point(1, 3 - dy)
                                ElseIf t(i) = "L" Then
                                    p1 = New Point(-1, 1)
                                    p2 = New Point(0, 1)
                                ElseIf t(i) = "R" Then
                                    p1 = New Point(2 - dx, 1)
                                    p2 = New Point(3 - dx, 1)
                                End If
                                connectors(UBound(connectors)) = New Point() {p1, p2}
                            ElseIf t(i) = "UB" Or t(i) = "DB" Or t(i) = "LB" Or t(i) = "RB" Then
                                Dim p1 As Point = Nothing
                                If t(i) = "UB" Then
                                    p1 = New Point(1, 0)
                                ElseIf t(i) = "DB" Then
                                    p1 = New Point(1, 2 - dy)
                                ElseIf t(i) = "LB" Then
                                    p1 = New Point(0, 1)
                                ElseIf t(i) = "RB" Then
                                    p1 = New Point(2 - dx, 1)
                                End If
                                boundary = p1
                            End If
                        Next i
                        If IsNothing(plateau) Then
                            ReDim plateau(0)
                        Else
                            ReDim Preserve plateau(plateau.Length)
                        End If
                        plateau(UBound(plateau)) = New Plateau With {.xSize = g.xSize, _
                                                                     .ySize = g.ySize, _
                                                                     .ground = g.ground, _
                                                                     .water = g.water, _
                                                                     .race = g.race, _
                                                                     .name = g.name, _
                                                                     .isWaterfall = isWaterfall, _
                                                                     .connectors = connectors, _
                                                                     .border = boundary}
                        maxPlateauSize = Math.Max(maxPlateauSize, Math.Max(g.xSize, g.ySize))
                    Else
                        For i As Integer = 0 To UBound(objType) Step 1
                            If i = UBound(objType) _
                            OrElse (k.Length > objType(i).Length AndAlso k.ToUpper.Substring(0, objType(i).Length) = objType(i).ToUpper) Then
                                If IsNothing(objList(i)) Then
                                    ReDim objList(i)(0)
                                Else
                                    ReDim Preserve objList(i)(objList(i).Length)
                                End If
                                objList(i)(UBound(objList(i))) = g
                                Exit For
                            End If
                        Next i
                    End If
                End If
            End If
        Next k
        mages = objList(0)
        mercenaries = objList(1)
        merchants = objList(2)
        mountains = objList(3)
        ruins = objList(4)
        trainers = objList(5)
        objects = objList(6)

        Dim races() As String = comm.TxtSplit(My.Resources.Races)
        For Each s As String In races
            Dim splited() As String = s.Split(CChar(" "))
            raceIdToString.Add(CInt(splited(UBound(splited))), splited(1).ToUpper)
        Next s

    End Sub

    ''' <summary>Определит ID объектов и настройки содержимого лавок</summary>
    ''' <param name="m">Карта со сгенерированными расами отрядов</param>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас.
    ''' Дробная часть определяет шанс округления большую сторону</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций. 
    ''' Значение количества объектов для каждой локации будет умножаться на отношение площади локации к площади средней локации (Pi*AverageRadius^2).
    ''' Дробная часть определяет шанс округления в большую сторону</param>
    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, ByRef settCommLoc As Map.SettingsLoc)

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")
        If Not settRaceLoc.isChecked Then Throw New Exception("Check parameters via settRaceLoc.Check()")
        If Not settCommLoc.isChecked Then Throw New Exception("Check parameters via settCommLoc.Check()")

        If Not m.complited.StacksRaceGen_Done Then
            Throw New Exception("Сначала нужно выполнить RaceGen.Gen")
        End If

        Dim t0 As Integer = Environment.TickCount

        Dim free(m.xSize, m.ySize) As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                free(x, y) = True
            Next x
        Next y

        Call PlaceCities(m, settMap)
        Call CorrectTownGuardsDesiredStats(m)
        Call PlaceAttendedObjects(m)
        Call PlaceMines(m, settMap, settRaceLoc, settCommLoc)
        Call PlacePlateau(m, free)
        Call PlaceMouintains(m, free)
        Call PlaceOtherObjects(m, free)
        Call AddSpells(m, settMap, settRaceLoc, settCommLoc)
        Call AddMercenaries(m, settMap, settRaceLoc, settCommLoc)
        Call AddMerchantItems(m, settMap, settRaceLoc, settCommLoc)

        Dim t1 As Integer = Environment.TickCount
        Console.WriteLine("Objects types definition: " & t1 - t0)

        m.complited.ImpenetrableObjectsPlacing_Done = True
    End Sub

    Private Function makeIDs(ByRef a() As MapObject) As List(Of Integer)
        Dim IDs As New List(Of Integer)
        For i As Integer = 0 To UBound(plateau) Step 1
            IDs.Add(i)
        Next i
        Return IDs
    End Function
    Private Function makeIDs(ByRef a() As Plateau, ByRef initOnly As Boolean, ByRef noWaterfalls As Boolean) As List(Of Integer)
        Dim IDs As New List(Of Integer)
        If initOnly Then
            For i As Integer = 0 To UBound(a) Step 1
                If a(i).connectors.Length = 1 Then IDs.Add(i)
            Next i
        Else
            For i As Integer = 0 To UBound(a) Step 1
                If (noWaterfalls And Not a(i).isWaterfall) Or Not noWaterfalls Then IDs.Add(i)
            Next i
        End If
        Return IDs
    End Function

    Private Function MayPlace(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, _
                          ByRef free(,) As Boolean, ByRef obj As MapObject) As Boolean
        Return MayPlace(m, x, y, free, obj.xSize, obj.ySize, obj.ground, obj.water, obj.race)
    End Function
    Private Function MayPlace(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, _
                              ByRef free(,) As Boolean, ByRef obj As Plateau, ByRef Connector(,) As Integer) As Boolean
        If Not IsNothing(obj.border) Then
            If obj.border.X = 0 Then
                If x > 0 Then Return False
            ElseIf obj.border.Y = 0 Then
                If y > 0 Then Return False
            ElseIf obj.border.X + 1 = obj.xSize Then
                If Not x + obj.border.X = m.xSize Then Return False
            ElseIf obj.border.Y + 1 = obj.ySize Then
                If Not y + obj.border.Y = m.ySize Then Return False
            End If
        End If
        Dim xx, yy As Integer
        For i As Integer = 0 To UBound(obj.connectors) Step 1
            For j As Integer = 0 To UBound(obj.connectors(i)) Step 1
                Call connectorPos(xx, yy, x, y, obj.connectors(i)(j))
                If xx < 0 Or yy < 0 Or xx > m.xSize Or yy > m.ySize Then Return False
            Next j
        Next i
        If Not IsNothing(Connector) Then
            Dim t As Boolean
            For i As Integer = 0 To UBound(obj.connectors) Step 1
                t = True
                For j As Integer = 0 To UBound(obj.connectors(i)) Step 1
                    Call connectorPos(xx, yy, x, y, obj.connectors(i)(j))
                    If Not Connector(xx, yy) = 1 Then
                        t = False
                        Exit For
                    End If
                Next j
                If t Then Exit For
            Next i
            If Not t Then Return False
        End If
        Return MayPlace(m, x, y, free, obj.xSize, obj.ySize, obj.ground, obj.water, obj.race)
    End Function
    Private Function MayPlace(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, ByRef free(,) As Boolean, _
                              ByRef xSize As Integer, ByRef ySize As Integer, _
                              ByRef isGround As Boolean, ByRef isWater As Boolean, _
                              ByRef races As List(Of Integer)) As Boolean
        If x + xSize - 1 > m.xSize Or y + ySize - 1 > m.ySize Then Return False
        Dim x2 As Integer = x + xSize - 1
        Dim y2 As Integer = y + ySize - 1
        If Not CheckRaces(m.board(x, y).objRace, races) Then Return False
        For j As Integer = y To y2 Step 1
            For i As Integer = x To x2 Step 1
                If Not m.board(i, j).isBorder Or Not free(i, j) Then Return False
                If Not CheckSurface(m.board(i, j).isWater, isGround, isWater) Then Return False
            Next i
        Next j
        Return True
    End Function
    Private Function CheckSurface(ByRef waterSurface As Boolean, ByRef isGround As Boolean, ByRef isWater As Boolean) As Boolean
        If waterSurface Then
            If Not isWater Then Return False
        Else
            If Not isGround Then Return False
        End If
        Return True
    End Function
    Private Function CheckRaces(ByRef mRaces As List(Of Integer), ByRef objRaces As List(Of Integer)) As Boolean
        For Each r As Integer In mRaces
            If objRaces.Contains(r) Then Return True
        Next r
        Return False
    End Function

    Private Function ObjectWeight(ByRef m As Map, ByRef obj As MapObject, _
                                  ByRef x As Integer, ByRef y As Integer) As Double
        Dim w As Double = 1
        Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 12)
        For p As Integer = b.minY To b.maxY Step 1
            For q As Integer = b.minX To b.maxX Step 1
                If obj.name = m.board(q, p).objectName Then w *= 0.5
            Next q
        Next p
        Return w
    End Function

    Private Sub PlaceAttendedObjects(ByRef m As Map)
        Dim objList() As MapObject
        Dim IDs As New List(Of Integer)
        Dim id As Integer
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).objectID > 2 And m.board(x, y).objectID < 8 Then
                    If m.board(x, y).objectID = 3 Then
                        objList = merchants
                    ElseIf m.board(x, y).objectID = 4 Then
                        objList = mercenaries
                    ElseIf m.board(x, y).objectID = 5 Then
                        objList = mages
                    ElseIf m.board(x, y).objectID = 6 Then
                        objList = trainers
                    ElseIf m.board(x, y).objectID = 7 Then
                        objList = ruins
                    Else
                        objList = Nothing
                    End If
                    IDs.Clear()
                    For i As Integer = 0 To UBound(objList) Step 1
                        If CheckSurface(m.board(x, y).isWater, objList(i).ground, objList(i).water) _
                        AndAlso CheckRaces(m.board(x, y).objRace, objList(i).race) Then
                            IDs.Add(i)
                        End If
                    Next i
                    If IDs.Count > 0 Then
                        id = comm.RandomSelection(IDs, True)
                    Else
                        id = -1
                    End If
                    m.board(x, y).objectName = objList(id).name
                End If
            Next x
        Next y
    End Sub
    Private Sub PlaceCities(ByRef m As Map, ByRef settMap As Map.SettingsMap)
        Dim cityGroup As Dictionary(Of Integer, List(Of Point)) = MakeCityGroupsList(m)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).objectID = 1 Then
                    If m.board(x, y).objRace.Item(0) = comm.RaceIdentifierToSubrace("H") Then
                        m.board(x, y).objectName = "G000FT0000HU0"
                    ElseIf m.board(x, y).objRace.Item(0) = comm.RaceIdentifierToSubrace("U") Then
                        m.board(x, y).objectName = "G000FT0000UN0"
                    ElseIf m.board(x, y).objRace.Item(0) = comm.RaceIdentifierToSubrace("C") Then
                        m.board(x, y).objectName = "G000FT0000DWC0"
                    ElseIf m.board(x, y).objRace.Item(0) = comm.RaceIdentifierToSubrace("L") Then
                        m.board(x, y).objectName = "G000FT0000HE0"
                    ElseIf m.board(x, y).objRace.Item(0) = comm.RaceIdentifierToSubrace("E") Then
                        m.board(x, y).objectName = "G000FT0000EL0"
                    End If
                End If
            Next x
        Next y
        If cityGroup.Count > 0 Then
            For Each L As List(Of Point) In cityGroup.Values
                Dim town As String
                If m.board(L.Item(0).X, L.Item(0).Y).locID.Item(0) <= settMap.nRaces Then
                    town = "G000FT0000NE1"
                Else
                    Dim r As Double = comm.rndgen.PRand(0, 1)
                    If r > 0.3 Then
                        town = "G000FT0000NE1"
                    ElseIf r > 0.1 Then
                        town = "G000FT0000NE2"
                    Else
                        town = "G000FT0000NE3"
                    End If
                End If
                For Each p As Point In L
                    m.board(p.X, p.Y).objectName = town
                Next p
            Next L
        End If
    End Sub
    Private Function MakeCityGroupsList(ByRef m As Map) As Dictionary(Of Integer, List(Of Point))
        Dim cityGroup As New Dictionary(Of Integer, List(Of Point))
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).objectID = 2 Then
                    Dim g As Integer = m.board(x, y).groupID
                    If Not cityGroup.ContainsKey(g) Then cityGroup.Add(g, New List(Of Point))
                    cityGroup.Item(g).Add(New Point(x, y))
                End If
            Next x
        Next y
        Return cityGroup
    End Function
    Private Sub CorrectTownGuardsDesiredStats(ByRef m As Map)
        Dim cityGroup As Dictionary(Of Integer, List(Of Point)) = MakeCityGroupsList(m)
        Dim s As AllDataStructues.DesiredStats
        Dim citySize As Integer
        Dim cityName As String
        For Each g As Integer In cityGroup.Keys
            If m.groupStats.ContainsKey(-g) Then
                s = AllDataStructues.DesiredStats.Copy(m.groupStats.Item(-g))
                cityName = m.board(cityGroup.Item(g).Item(0).X, cityGroup.Item(g).Item(0).Y).objectName
                citySize = CInt(cityName.Substring(cityName.Length - 1))
                If s.StackSize > citySize Then
                    s.ExpBarAverage = CInt((s.ExpBarAverage * s.StackSize) / citySize)
                    s.StackSize = citySize
                    If s.StackSize < 2 Then
                        s.MaxGiants = 0
                    ElseIf s.StackSize < 4 Then
                        s.MaxGiants = Math.Min(s.MaxGiants, 1)
                    ElseIf s.StackSize < 6 Then
                        s.MaxGiants = Math.Min(s.MaxGiants, 2)
                    Else
                        s.MaxGiants = Math.Min(s.MaxGiants, 3)
                    End If
                End If
                s.isInternalCityGuard = True
                m.groupStats.Remove(-g)
                m.groupStats.Add(-g, AllDataStructues.DesiredStats.Copy(s))
            End If
        Next g
    End Sub
    Private Sub PlaceMines(ByRef m As Map, ByRef settMap As Map.SettingsMap, _
                           ByRef settRaceLoc As Map.SettingsLoc, ByRef settCommLoc As Map.SettingsLoc)
        Dim mineType(m.xSize, m.ySize) As String
        For i As Integer = 0 To UBound(m.Loc) Step 1
            If Not m.Loc(i).IsObtainedBySymmery Then
                If i < settMap.nRaces Then
                    Call PlaceMinesInRaceLoc(m, settMap, settRaceLoc, m.Loc(i), mineType)
                Else
                    Call PlaceMinesInCommonLoc(m, settMap, settCommLoc, m.Loc(i), mineType)
                End If
            End If
        Next i
        'установить для шахт конкретный вид ресурсов
        Dim raceMana As Dictionary(Of Integer, AllDataStructues.Cost()) = RacesManaUsing()
        Dim raceManaTier As Dictionary(Of Integer, String()) = ManaTier(raceMana)
        Dim mines As New Dictionary(Of String, String)
        Dim IDs As New List(Of Integer)
        mines.Add("gold", "G000CR0000GL")
        mines.Add("green", "G000CR0000GR")
        mines.Add("black", "G000CR0000RG")
        mines.Add("white", "G000CR0000WH")
        mines.Add("red", "G000CR0000RD")
        mines.Add("blue", "G000CR0000YE")
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If Not mineType(x, y) = "" Then
                    Dim types() As String = Nothing
                    Dim weights() As Double = Nothing
                    If mineType(x, y) = My.Resources.mineTypeGold Then
                        types = New String() {"gold"}
                        weights = New Double() {1}
                    ElseIf mineType(x, y) = My.Resources.mineTypeT1Mana Or mineType(x, y) = My.Resources.mineTypeT3Mana Then
                        Dim tier As Integer
                        If mineType(x, y) = My.Resources.mineTypeT1Mana Then
                            tier = 1
                        Else
                            tier = 3
                        End If
                        types = raceManaTier(m.board(x, y).objRace(0))(tier).Split(CChar(" "))
                        ReDim weights(UBound(types))
                        For i As Integer = 0 To UBound(types) Step 1
                            If types(i) = "gold" Then
                                weights(i) = raceMana(m.board(x, y).objRace(0))(tier).Gold
                            ElseIf types(i) = "green" Then
                                weights(i) = raceMana(m.board(x, y).objRace(0))(tier).Green
                            ElseIf types(i) = "black" Then
                                weights(i) = raceMana(m.board(x, y).objRace(0))(tier).Black
                            ElseIf types(i) = "white" Then
                                weights(i) = raceMana(m.board(x, y).objRace(0))(tier).White
                            ElseIf types(i) = "red" Then
                                weights(i) = raceMana(m.board(x, y).objRace(0))(tier).Red
                            ElseIf types(i) = "blue" Then
                                weights(i) = raceMana(m.board(x, y).objRace(0))(tier).Blue
                            End If
                        Next i
                    ElseIf mineType(x, y) = My.Resources.mineTypeRandomMana Then
                        types = New String() {My.Resources.mineTypeRandomMana}
                        weights = New Double() {1}
                    End If
                    IDs.Clear()
                    For i As Integer = 0 To UBound(types) Step 1
                        IDs.Add(i)
                    Next i
                    Dim r As Integer = comm.RandomSelection(IDs, True)
                    m.board(x, y).objectName = types(r)
                End If
            Next x
        Next y
    End Sub
    Private Sub PlaceMinesInRaceLoc(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, _
                                    ByRef Loc As Location, ByRef mineType(,) As String)
        Dim ListPos As New List(Of Point)
        Dim capitalPos As Point = Nothing
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID.Item(0) = Loc.ID Then
                    If m.board(x, y).objectID = 8 Then
                        ListPos.Add(New Point(x, y))
                    ElseIf m.board(x, y).objectID = 1 Then
                        capitalPos = New Point(x, y)
                    End If
                End If
            Next x
        Next y
        Dim pos() As Point = ListPos.ToArray
        Dim dist(UBound(pos)) As Integer
        For i As Integer = 0 To UBound(pos) Step 1
            dist(i) = capitalPos.SqDist(pos(i))
        Next i
        Array.Sort(dist, pos)
        Dim goldLimit As Double = CDbl(pos.Length) * settRaceLoc.maxGoldMines / (settRaceLoc.maxGoldMines + settRaceLoc.maxManaSources + 0.001)
        Dim manaLimit As Double = CDbl(pos.Length) * settRaceLoc.maxManaSources / (settRaceLoc.maxGoldMines + settRaceLoc.maxManaSources + 0.001)
        Dim manaCounter As Integer = 0
        For i As Integer = 0 To UBound(pos) Step 1
            If i = 0 And goldLimit > 0 Then
                Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeGold)
                goldLimit -= 1
            ElseIf i = 1 And manaLimit > 0 Then
                Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeT1Mana)
                manaLimit -= 1
                manaCounter += 1
            Else
                If PlaceGoldMine(goldLimit, manaLimit, settRaceLoc) Then
                    Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeGold)
                    goldLimit -= 1
                Else
                    If manaCounter > 1 And settMap.spellsMaxLevel > 2 Then
                        Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeT3Mana)
                        manaCounter = 0
                    Else
                        Call SetMineType(m, settMap.nRaces, mineType, pos(i), My.Resources.mineTypeT1Mana)
                        manaCounter += 1
                    End If
                    manaLimit -= 1
                End If
            End If
        Next i
    End Sub
    Private Sub PlaceMinesInCommonLoc(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settCommLoc As Map.SettingsLoc, _
                                      ByRef Loc As Location, ByRef mineType(,) As String)
        Dim ListPos As New List(Of Point)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).locID.Item(0) = Loc.ID And m.board(x, y).objectID = 8 Then ListPos.Add(New Point(x, y))
            Next x
        Next y
        Dim goldLimit As Double = CDbl(ListPos.Count) * settCommLoc.maxGoldMines / (settCommLoc.maxGoldMines + settCommLoc.maxManaSources + 0.001)
        Dim manaLimit As Double = CDbl(ListPos.Count) * settCommLoc.maxManaSources / (settCommLoc.maxGoldMines + settCommLoc.maxManaSources + 0.001)
        Dim r1 As Double
        Dim r2 As Double = settCommLoc.maxGoldMines / (settCommLoc.maxManaSources + 0.001)
        For Each p As Point In ListPos
            r1 = goldLimit / (manaLimit + 0.001)
            If PlaceGoldMine(goldLimit, manaLimit, settCommLoc) Then
                Call SetMineType(m, settMap.nRaces, mineType, p, My.Resources.mineTypeGold)
                goldLimit -= 1
            Else
                Call SetMineType(m, settMap.nRaces, mineType, p, My.Resources.mineTypeRandomMana)
                manaLimit -= 1
            End If
        Next p
    End Sub
    Private Function PlaceGoldMine(ByRef goldLimit As Double, ByRef manaLimit As Double, ByRef settLoc As Map.SettingsLoc) As Boolean
        If goldLimit > 0 Then
            Dim r1 As Double = goldLimit / (manaLimit + 0.001)
            Dim r2 As Double = settLoc.maxGoldMines / (settLoc.maxManaSources + 0.001)
            If goldLimit > 0 And manaLimit > 0 And goldLimit + manaLimit < 1 Then
                Dim R As Double = comm.rndgen.PRand(0, goldLimit + manaLimit)
                Return R < settLoc.maxGoldMines
            Else
                If r1 > r2 Or manaLimit <= 0 Then
                    Return True
                Else
                    Return False
                End If
            End If
        Else
            If manaLimit > 0 Then
                Return False
            Else
                Dim R As Double = comm.rndgen.PRand(0, settLoc.maxGoldMines + settLoc.maxManaSources)
                Return R < settLoc.maxGoldMines
            End If
        End If
    End Function
    Private Sub SetMineType(ByRef m As Map, ByRef nRaces As Integer, ByRef mineType(,) As String, _
                            ByRef p As Point, ByRef type As String)
        'Console.WriteLine(type)
        If m.symmID > -1 Then
            Dim pp() As Point = symm.ApplySymm(p, nRaces, m, 1)
            For i As Integer = 0 To UBound(pp) Step 1
                mineType(pp(i).X, pp(i).Y) = type
            Next i
        Else
            mineType(p.X, p.Y) = type
        End If
    End Sub
    Private Function RacesManaUsing() As Dictionary(Of Integer, AllDataStructues.Cost())
        Dim r() As String = comm.TxtSplit(My.Resources.Races)
        Dim res As New Dictionary(Of Integer, AllDataStructues.Cost())
        For i As Integer = 0 To UBound(r) Step 1
            Dim s() As String = r(i).Split(CChar(" "))
            res.Add(comm.RaceIdentifierToSubrace(s(UBound(s))), New AllDataStructues.Cost() {Nothing, Nothing, Nothing, Nothing, Nothing, Nothing})
        Next i
        For s As Integer = 0 To UBound(raceSpells) Step 1
            If raceSpells(s).researchCost.Count > 0 Then
                For Each L As String In raceSpells(s).researchCost.Keys
                    Dim LRace As Integer = comm.LordsRace.Item(L)
                    res.Item(LRace)(raceSpells(s).level) = raceSpells(s).researchCost.Item(L) + raceSpells(s).castCost
                Next L
            End If
        Next s
        Dim remove As New List(Of Integer)
        For Each i As Integer In res.Keys
            For j As Integer = 1 To 5 Step 1
                res.Item(i)(0) += res.Item(i)(j)
            Next j
            If AllDataStructues.Cost.Sum(res.Item(i)(0)) = 0 Then remove.Add(i)
        Next i
        For Each i As Integer In remove
            res.Remove(i)
        Next i
        Return res
    End Function
    Private Function ManaTier(ByRef raceMana As Dictionary(Of Integer, AllDataStructues.Cost())) As Dictionary(Of Integer, String())
        Dim res As New Dictionary(Of Integer, String())
        For Each i As Integer In raceMana.Keys
            res.Add(i, New String() {"", "", "", "", "", ""})
            For j As Integer = 1 To UBound(raceMana.Item(i)) Step 1
                If raceMana.Item(i)(j).Black > 0 Then res.Item(i)(j) &= "black "
                If raceMana.Item(i)(j).Blue > 0 Then res.Item(i)(j) &= "blue "
                If raceMana.Item(i)(j).Gold > 0 Then res.Item(i)(j) &= "gold "
                If raceMana.Item(i)(j).Green > 0 Then res.Item(i)(j) &= "green "
                If raceMana.Item(i)(j).Red > 0 Then res.Item(i)(j) &= "red "
                If raceMana.Item(i)(j).White > 0 Then res.Item(i)(j) &= "while "
                res.Item(i)(j) = res.Item(i)(j).Remove(res.Item(i)(j).Length - 1)
            Next j
            For j As Integer = 1 To UBound(raceMana.Item(i)) - 1 Step 1
                Dim s() As String = res.Item(i)(j).Split(CChar(" "))
                For q As Integer = 0 To UBound(s) Step 1
                    For k As Integer = j + 1 To UBound(raceMana.Item(i)) Step 1
                        If s(q).Length > 0 Then res.Item(i)(k) = res.Item(i)(k).Replace(s(q), "")
                    Next k
                Next q
            Next j
            For j As Integer = 1 To UBound(raceMana.Item(i)) Step 1
                res.Item(i)(j) = res.Item(i)(j).Trim
            Next j
        Next i
        Return res
    End Function

    Private Sub PlacePlateau(ByRef m As Map, ByRef free(,) As Boolean)
        Dim connectors(m.xSize, m.ySize) As Integer
        Dim IDs As List(Of Integer)
        Dim id As Integer
        Dim ok As Boolean
        For i As Integer = 0 To 2 * m.Loc.Length Step 1
            IDs = makeIDs(plateau, True, False)
            Do While IDs.Count > 0
                id = comm.RandomSelection(IDs, True)
                ok = TryToPlace(m, plateau(id), free, connectors)
                If ok Then
                    Exit Do
                Else
                    IDs.Remove(id)
                End If
            Loop
            If IDs.Count = 0 Then Exit For
        Next i
    End Sub
    Private Function TryToPlace(ByRef m As Map, ByRef basic As Plateau, _
                                ByRef free(,) As Boolean, ByRef connectors(,) As Integer) As Boolean

        'сначала пытаемся расположить базовый кусок (в каждом из возможных положений, пока не получится построить цепочку)
        'после успешного расположения базового куска пытаемся построить от него цепочку
        'если все получилось - возвращаем true
        Dim places As New List(Of Point)
        Dim res As New PlateauPlacingResult With {.n = -1}
        ReDim res.obj(maxChainLen), res.pos(maxChainLen)
        Dim id As Integer
        Dim ok As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If MayPlace(m, x, y, free, basic, Nothing) Then places.Add(New Point(x, y))
            Next x
        Next y

        Do While places.Count > 0
            id = comm.rndgen.RndPos(places.Count, True) - 1
            ok = MakeChain(m, basic, places(id).X, places(id).Y, free, connectors, res)
            If ok Then
                For i As Integer = 0 To res.n Step 1
                    m.board(res.pos(i).X, res.pos(i).Y).objectName = res.obj(i).name
                Next i
                Return True
            Else
                places.RemoveAt(id)
            End If
        Loop
        Return False
    End Function

    Private Sub PlaceObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As MapObject)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, False)
    End Sub
    Private Sub PlaceObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As Plateau, ByRef connectors(,) As Integer)
        Call PlaceObject(free, x, y, obj, connectors, 1)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, False)
    End Sub
    Private Sub RemoveObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As MapObject)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, True)
    End Sub
    Private Sub RemoveObject(ByRef free(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef obj As Plateau, ByRef connectors(,) As Integer)
        Call PlaceObject(free, x, y, obj.xSize, obj.ySize, True)
        Call PlaceObject(free, x, y, obj, connectors, -1)
    End Sub
    Private Sub PlaceObject(ByRef free(,) As Boolean, _
                            ByRef x As Integer, ByRef y As Integer, _
                            ByRef xSize As Integer, ByRef ySize As Integer, _
                            ByRef whatSet As Boolean)
        Dim x1 As Integer = x + xSize - 1
        Dim y1 As Integer = y + ySize - 1
        For j As Integer = y To y1 Step 1
            For i As Integer = x To x1 Step 1
                free(i, j) = whatSet
            Next i
        Next j
    End Sub
    Private Sub PlaceObject(ByRef free(,) As Boolean, _
                            ByRef x As Integer, ByRef y As Integer, _
                            ByRef obj As Plateau, ByRef connectors(,) As Integer, _
                            ByRef whatAdd As Integer)
        Dim xx, yy As Integer
        For i As Integer = 0 To UBound(obj.connectors) Step 1
            For j As Integer = 0 To UBound(obj.connectors(i)) Step 1
                Call connectorPos(xx, yy, x, y, obj.connectors(i)(j))
                connectors(xx, yy) += whatAdd
            Next j
        Next i
    End Sub
    Private Sub connectorPos(ByRef xOut As Integer, ByRef yOut As Integer, ByRef x As Integer, ByRef y As Integer, _
                             ByRef connectorRelativePos As Point)
        xOut = x + connectorRelativePos.X
        yOut = y + connectorRelativePos.Y
        'If connectorRelativePos.X > 0 Then xOut += obj.xSize - 1
        'If connectorRelativePos.Y > 0 Then yOut += obj.ySize - 1
    End Sub
    Private Function MakeChain(ByRef m As Map, ByRef obj As Plateau, _
                               ByRef x As Integer, ByRef y As Integer, _
                               ByRef free(,) As Boolean, ByRef connectors(,) As Integer, _
                               ByRef res As PlateauPlacingResult) As Boolean
        res.n += 1
        res.obj(res.n) = obj
        res.pos(res.n) = New Point(x, y)

        Call PlaceObject(free, x, y, obj, connectors)
        Dim x1 As Integer = Math.Max(x - maxPlateauSize, 0)
        Dim y1 As Integer = Math.Max(y - maxPlateauSize, 0)
        Dim x2 As Integer = Math.Min(x + obj.xSize + maxPlateauSize - 1, m.xSize)
        Dim y2 As Integer = Math.Min(y + obj.ySize + maxPlateauSize - 1, m.ySize)
        Dim placeMore As Boolean = False
        For j As Integer = y1 To y2 Step 1
            For i As Integer = x1 To x2 Step 1
                If connectors(i, j) = 1 Then
                    placeMore = True
                    i = x2
                    j = y2
                End If
            Next i
        Next j
        If Not placeMore Then Return True

        Dim IDs As List(Of Integer)
        Dim id, nx, ny As Integer
        If res.n < maxChainLen Then
            IDs = makeIDs(plateau, False, True)
        Else
            IDs = makeIDs(plateau, True, True)
        End If

        Do While IDs.Count > 0
            id = comm.rndgen.RndPos(IDs.Count, True) - 1
            nx = -1
            For j As Integer = y1 To y2 Step 1
                For i As Integer = x1 To x2 Step 1
                    If MayPlace(m, i, j, free, plateau(id), connectors) Then
                        nx = i
                        ny = j
                        i = x2
                        j = y2
                    End If
                Next i
            Next j
            If nx > -1 Then
                If MakeChain(m, plateau(id), nx, ny, free, connectors, res) Then
                    Return True
                Else
                    IDs.RemoveAt(id)
                End If
            Else
                IDs.RemoveAt(id)
            End If
        Loop
        Call RemoveObject(free, res.pos(res.n).X, res.pos(res.n).Y, res.obj(res.n), connectors)
        res.obj(res.n) = Nothing
        res.pos(res.n) = Nothing
        res.n -= 1
        Return False
    End Function

    Private Sub PlaceMouintains(ByRef m As Map, ByRef free(,) As Boolean)

        Dim Tmp(,) As Integer
        Dim sizeslist, IDs As New List(Of Integer)
        Dim pos As New List(Of Point)
        Dim tmpPos() As List(Of Point)
        For Each obj As MapObject In mountains
            If Not sizeslist.Contains(obj.xSize) Then sizeslist.Add(obj.xSize)
        Next obj
        Dim basicSize(sizeslist.Count - 1) As Integer
        Dim weight(UBound(basicSize)) As Double
        For s As Integer = 0 To sizeslist.Count - 1 Step 1
            basicSize(s) = sizeslist.Item(s)
            weight(s) = 1 / basicSize(s) ^ 2
        Next s
        sizeslist = Nothing
        Array.Sort(basicSize)
        Dim maxR As Integer
        Dim ismountain(m.xSize, m.ySize) As Boolean

        For s As Integer = UBound(basicSize) To 0 Step -1
            If basicSize(s) > 1 Then
                Dim n As Integer = Math.Max(1, CInt(0.5 * UBound(m.Loc))) * Math.Max(1, 6 - basicSize(s))
                For attempt As Integer = 0 To n Step 1
                    ReDim Tmp(2 * (m.xSize + 1), 2 * (m.ySize + 1))
                    ReDim tmpPos(UBound(Tmp, 2))
                    For y As Integer = 0 To UBound(Tmp, 2) Step 1
                        tmpPos(y) = New List(Of Point)
                    Next y
                    Call PlaceTmpMountain(basicSize(s), m.xSize, m.ySize, Tmp)
                    Dim minX, maxX, minY, maxY As Integer
                    minX = m.xSize : minY = m.ySize
                    maxX = minX + basicSize(s) - 1 : maxY = minY + basicSize(s) - 1

                    maxR = s
                    IDs.Clear()
                    For i As Integer = 0 To maxR - 1 Step 1
                        IDs.Add(i)
                    Next i
                    Dim k As Integer = 0
                    Do While k < 7
                        k += 1
                        Dim r As Integer = comm.RandomSelection(IDs, weight, True) 'comm.rndgen.RndPos(maxR, True) - 1
                        Parallel.For(0, UBound(Tmp, 2) + 1, _
                         Sub(y As Integer)
                             tmpPos(y).Clear()
                             For x As Integer = 0 To UBound(Tmp, 1) Step 1
                                 If MayPlaceMountainBlock(basicSize(r), x, y, Tmp, minX, maxX, minY, maxY) Then
                                     tmpPos(y).Add(New Point(x, y))
                                 End If
                             Next x
                         End Sub)
                        pos.Clear()
                        For y As Integer = 0 To UBound(Tmp, 2) Step 1
                            For Each p As Point In tmpPos(y)
                                pos.Add(p)
                            Next p
                        Next y
                        If pos.Count > 0 Then
                            Dim selected As Integer = comm.rndgen.RndPos(pos.Count, True) - 1
                            Call PlaceTmpMountain(basicSize(r), pos.Item(selected).X, pos.Item(selected).Y, Tmp)
                            minX = Math.Min(minX, pos.Item(selected).X)
                            minY = Math.Min(minY, pos.Item(selected).Y)
                            maxX = Math.Max(maxX, pos.Item(selected).X + basicSize(r) - 1)
                            maxY = Math.Max(maxY, pos.Item(selected).Y + basicSize(r) - 1)
                        Else
                            If r = 0 Then
                                Exit Do
                            Else
                                maxR = r
                                IDs.Clear()
                                For i As Integer = 0 To maxR - 1 Step 1
                                    IDs.Add(i)
                                Next i
                            End If
                        End If
                    Loop

                    pos.Clear()
                    For y As Integer = 0 To m.ySize Step 1
                        For x As Integer = 0 To m.xSize Step 1
                            If MayPlaceMountainBlock(m, free, x, y, Tmp, minX, maxX, minY, maxY) Then pos.Add(New Point(x, y))
                        Next x
                    Next y
                    If pos.Count > 0 Then
                        Dim selected As Integer = comm.rndgen.RndPos(pos.Count, True) - 1
                        Call PlaceMountainBlock(m, free, pos(selected).X, pos(selected).Y, Tmp, minX, maxX, minY, maxY, ismountain)
                    End If
                Next attempt
            End If
        Next s
        For s As Integer = UBound(basicSize) To 0 Step -1
            If basicSize(s) < 4 Then
                Dim j1 As Integer = m.ySize
                Dim j2 As Integer = 0
                Dim i1 As Integer = m.xSize
                Dim i2 As Integer = 0
                Dim s1 As Integer = -1
                For a As Integer = 0 To 3 Step 1
                    j1 = m.ySize - j1
                    j2 = m.ySize - j2
                    i1 = m.xSize - i1
                    i2 = m.xSize - i2
                    s1 = -s1
                    For j As Integer = j1 To j2 Step s1
                        For i As Integer = i1 To i2 Step s1
                            If MayPlaceSingleMountain(m, free, i, j, basicSize(s), ismountain) Then
                                Call PlaceSingleMountain(m, free, i, j, basicSize(s), ismountain)
                            End If
                        Next i
                    Next j
                Next a
            End If
        Next s
    End Sub
    Private Sub PlaceTmpMountain(ByRef size As Integer, ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer)
        For j As Integer = y To y + size - 1 Step 1
            For i As Integer = x To x + size - 1 Step 1
                tmp(i, j) = size
            Next i
        Next j
    End Sub
    Private Function MayPlaceMountainBlock(ByRef size As Integer, ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer, _
                                           ByRef minX As Integer, ByRef maxX As Integer, _
                                           ByRef minY As Integer, ByRef maxY As Integer) As Boolean
        If x + size - 1 > UBound(tmp, 1) Then Return False
        If y + size - 1 > UBound(tmp, 2) Then Return False

        If x + size < minX Then Return False
        If y + size < minY Then Return False
        If x > maxX + 1 Then Return False
        If y > maxY + 1 Then Return False

        Dim t As Boolean = False
        Dim e As Boolean = False
        For j As Integer = y To y + size - 1 Step 1
            For i As Integer = x To x + size - 1 Step 1
                If tmp(i, j) > 0 Then
                    Return False
                ElseIf Not t Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, UBound(tmp, 1), UBound(tmp, 2), 1)
                    For p As Integer = b.minY To b.maxY Step 1
                        For q As Integer = b.minX To b.maxX Step 1
                            If tmp(q, p) > size Then
                                t = True
                                Exit For
                            ElseIf tmp(q, p) = size Then
                                e = True
                            End If
                        Next q
                        If t Then Exit For
                    Next p
                End If
            Next i
        Next j
        If Not t And e AndAlso comm.rndgen.PRand(0, 1) < 0.01 Then Return True
        Return t
    End Function
    Private Function MayPlaceMountainBlock(ByRef m As Map, ByRef free(,) As Boolean, _
                                           ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer, _
                                           ByRef minX As Integer, ByRef maxX As Integer, _
                                           ByRef minY As Integer, ByRef maxY As Integer) As Boolean
        Dim dx As Integer = maxX - minX
        Dim dy As Integer = maxY - minY
        If x + dx > m.xSize Then Return False
        If y + dy > m.ySize Then Return False
        For j As Integer = 0 To dy Step 1
            For i As Integer = 0 To dx Step 1
                If tmp(minX + i, minY + j) > 0 Then
                    If Not TestCellForMountain(m, free, x + i, y + j) Then Return False
                End If
            Next i
        Next j
        Return True
    End Function
    Private Function TestCellForMountain(ByRef m As Map, ByRef free(,) As Boolean, _
                                         ByRef x As Integer, ByRef y As Integer) As Boolean
        If Not free(x, y) Then Return False
        If Not m.board(x, y).isBorder Then Return False
        'If m.board(x, y).isWater Then Return False
        Return True
    End Function
    Private Sub PlaceMountainBlock(ByRef m As Map, ByRef free(,) As Boolean, _
                                   ByRef x As Integer, ByRef y As Integer, tmp(,) As Integer, _
                                   ByRef minX As Integer, ByRef maxX As Integer, _
                                   ByRef minY As Integer, ByRef maxY As Integer, _
                                   ByRef ismountain(,) As Boolean)
        Dim dx As Integer = maxX - minX
        Dim dy As Integer = maxY - minY
        Dim IDs As New List(Of Integer)
        For j As Integer = 0 To dy Step 1
            For i As Integer = 0 To dx Step 1
                If tmp(minX + i, minY + j) > 0 Then
                    Dim size As Integer = tmp(minX + i, minY + j)
                    For p As Integer = 0 To size - 1 Step 1
                        For q As Integer = 0 To size - 1 Step 1
                            tmp(minX + i + q, minY + j + p) = 0
                        Next q
                    Next p
                    Call PlaceSingleMountain(m, free, x + i, y + j, size, ismountain)
                End If
            Next i
        Next j
    End Sub
    Private Function MayPlaceSingleMountain(ByRef m As Map, ByRef free(,) As Boolean, _
                                            ByRef x As Integer, ByRef y As Integer, _
                                            ByRef size As Integer, _
                                            ByRef ismountain(,) As Boolean) As Boolean
        If x + size - 1 > m.xSize Then Return False
        If y + size - 1 > m.ySize Then Return False
        Dim t As Boolean = False
        For j As Integer = y To y + size - 1 Step 1
            For i As Integer = x To x + size - 1 Step 1
                If Not TestCellForMountain(m, free, i, j) Then Return False
                If Not t Then
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(i, j, m.xSize, m.ySize, 1)
                    For p As Integer = b.minY To b.maxY Step 1
                        For q As Integer = b.minX To b.maxX Step 1
                            If ismountain(q, p) Then
                                t = True
                                Exit For
                            End If
                        Next q
                        If t Then Exit For
                    Next p
                End If
            Next i
        Next j
        If t Then
            Dim r As Double = comm.rndgen.PRand(0, 1)
            Dim d As Double = Math.Max(0.075 - 0.005 * size, 0.025)
            Return r < d
        Else
            Return False
        End If
    End Function
    Private Sub PlaceSingleMountain(ByRef m As Map, ByRef free(,) As Boolean, _
                                    ByRef x As Integer, ByRef y As Integer, _
                                    ByRef size As Integer, _
                                    ByRef ismountain(,) As Boolean)
        For p As Integer = 0 To size - 1 Step 1
            For q As Integer = 0 To size - 1 Step 1
                free(x + q, y + p) = False
                m.board(x + q, y + p).isWater = False
                ismountain(x + q, y + p) = True
            Next q
        Next p
        Dim IDs As New List(Of Integer)
        Dim weight(UBound(mountains)) As Double
        For n As Integer = 0 To UBound(mountains) Step 1
            If mountains(n).xSize = size AndAlso CheckRaces(m.board(x, y).objRace, mountains(n).race) Then
                IDs.Add(n)
                weight(n) = ObjectWeight(m, mountains(n), x, y)
            End If
        Next n
        Dim selected As Integer = comm.RandomSelection(IDs, weight, True)
        m.board(x, y).objectName = mountains(selected).name
    End Sub

    Private Sub PlaceOtherObjects(ByRef m As Map, ByRef free(,) As Boolean)
        Dim IDs As New List(Of Integer)
        Dim weight(UBound(objects)) As Double
        Dim nextloop As Boolean = True
        Dim k As Integer = 0
        Dim f(,) As Boolean = free
        Dim tmpm As Map = m
        Do While nextloop And k < 5
            nextloop = False
            For y As Integer = 0 To tmpm.ySize Step 1
                For x As Integer = 0 To tmpm.xSize Step 1
                    If f(x, y) And tmpm.board(x, y).isBorder Then
                        nextloop = True
                        IDs.Clear()
                        Dim xx As Integer = x
                        Dim yy As Integer = y
                        Parallel.For(0, objects.Length, _
                         Sub(n As Integer)
                             If MayPlace(tmpm, xx, yy, f, objects(n)) Then
                                 weight(n) = ObjectWeight(tmpm, objects(n), xx, yy)
                             Else
                                 weight(n) = -1
                             End If
                         End Sub)
                        For n As Integer = 0 To UBound(objects) Step 1
                            If weight(n) >= 0 Then IDs.Add(n)
                        Next n
                        If IDs.Count > 0 Then
                            Dim selected As Integer = comm.RandomSelection(IDs, weight, True)
                            tmpm.board(x, y).objectName = objects(selected).name
                            Call PlaceObject(f, x, y, objects(selected))
                        End If
                    End If
                Next x
            Next y
            k += 1
        Loop
        Parallel.For(0, tmpm.ySize + 1,
         Sub(y As Integer)
             For x As Integer = 0 To tmpm.xSize Step 1
                 If f(x, y) And tmpm.board(x, y).isBorder Then
                     If IDs.Count > 0 Then
                         f(x, y) = False
                         tmpm.board(x, y).objectName = "MOMNE0100"
                     End If
                 End If
             Next x
         End Sub)
        free = f
        m = tmpm
    End Sub

    Private Sub AddSpells(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, ByRef settCommLoc As Map.SettingsLoc)
        Dim maxGroupID As Integer = ImpenetrableMeshGen.GetMaxGroupID(m)
        Dim objList As Dictionary(Of Integer, List(Of Point)) = FindGroupsOfObjects(m, 5)
        Dim levels As List(Of Integer)
        Dim r, n As Integer
        Dim setNewGroup As Boolean
        For Each g As Integer In objList.Keys
            If pointLoc(m, objList.Item(g).Item(0)) <= settMap.nRaces Then
                setNewGroup = False
                levels = SelectSpellsLevel(settRaceLoc)
                For Each p As Point In objList.Item(g)
                    r = m.board(p.X, p.Y).objRace.Item(0)
                    If setNewGroup Then
                        maxGroupID += 1
                        n = maxGroupID
                        m.board(p.X, p.Y).groupID = maxGroupID
                    Else
                        setNewGroup = True
                        n = g
                    End If
                    m.groupStats.Add(n, New AllDataStructues.DesiredStats With {.shopContent = SelectSpells(settRaceLoc, r, levels)})
                Next p
            Else
                levels = SelectSpellsLevel(settCommLoc)
                m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectSpells(settCommLoc, -1, levels)})
            End If
        Next g
    End Sub
    Private Function SelectSpells(ByRef settLoc As Map.SettingsLoc, ByRef raceID As Integer, ByRef levels As List(Of Integer)) As List(Of String)
        Dim res As New List(Of String)
        Dim v, r As String
        If raceID = -1 Then
            r = "R"
        Else
            r = raceIdToString.Item(raceID)
        End If
        For Each L As Integer In levels
            v = L.ToString & r
            If settLoc.mageGlobalSpellsEnabled Then
                v &= "T"
            Else
                v &= "F"
            End If
            res.Add(v)
        Next L
        Return res
    End Function
    Private Function SelectSpellsLevel(ByRef settLoc As Map.SettingsLoc) As List(Of Integer)
        Dim res As New List(Of Integer)
        For i As Integer = 1 To settLoc.mageSpellsCount Step 1
            res.Add(RndInt(settLoc.mageSpellsMinLevel, settLoc.mageSpellsMaxLevel))
        Next i
        Return res
    End Function
    Private Function FindGroupsOfObjects(ByRef m As Map, ByRef objType As Integer) As Dictionary(Of Integer, List(Of Point))
        Dim res As New Dictionary(Of Integer, List(Of Point))
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If m.board(x, y).objectID = objType Then
                    If Not res.ContainsKey(m.board(x, y).groupID) Then res.Add(m.board(x, y).groupID, New List(Of Point))
                    res.Item(m.board(x, y).groupID).Add(New Point(x, y))
                End If
            Next x
        Next y
        Return res
    End Function
    Private Function RndInt(ByRef min As Integer, ByRef max As Integer) As Integer
        Return min - 1 + comm.rndgen.RndPos(max - min + 1, True)
    End Function
    Private Function pointLoc(ByRef m As Map, ByRef p As Point) As Integer
        Return m.board(p.X, p.Y).locID.Item(0)
    End Function

    Private Sub AddMercenaries(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, ByRef settCommLoc As Map.SettingsLoc)
        Dim objList As Dictionary(Of Integer, List(Of Point)) = FindGroupsOfObjects(m, 4)
        For Each g As Integer In objList.Keys
            If pointLoc(m, objList.Item(g).Item(0)) <= settMap.nRaces Then
                m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectMercenaries(settRaceLoc)})
            Else
                m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectMercenaries(settCommLoc)})
            End If
        Next g
    End Sub
    Private Function SelectMercenaries(ByRef settLoc As Map.SettingsLoc) As List(Of String)
        Dim res As New List(Of String)
        For i As Integer = 1 To settLoc.mercenariesCount Step 1
            res.Add(RndInt(settLoc.mercenariesMinExpBar, settLoc.mercenariesMaxExpBar).ToString)
        Next i
        Return res
    End Function

    Private Sub AddMerchantItems(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef settRaceLoc As Map.SettingsLoc, ByRef settCommLoc As Map.SettingsLoc)
        Dim objList As Dictionary(Of Integer, List(Of Point)) = FindGroupsOfObjects(m, 3)
        For Each g As Integer In objList.Keys
            If pointLoc(m, objList.Item(g).Item(0)) <= settMap.nRaces Then
                m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectMerchantItems(settRaceLoc)})
            Else
                m.groupStats.Add(g, New AllDataStructues.DesiredStats With {.shopContent = SelectMerchantItems(settCommLoc)})
            End If
        Next g
    End Sub
    Private Function SelectMerchantItems(ByRef settLoc As Map.SettingsLoc) As List(Of String)
        Dim res As New List(Of String)
        Dim sum, v As Integer
        If settLoc.merchMinItemCost + settLoc.merchMaxItemCost = 0 Then Return res
        Do While sum < settLoc.merchItemsCost
            v = RndInt(settLoc.merchMinItemCost, settLoc.merchMaxItemCost)
            If sum + v > settLoc.merchItemsCost Then v = settLoc.merchItemsCost - sum
            sum += v
            res.Add(v.ToString)
        Loop
        Return res
    End Function

End Class

Public Class ObjectsContentSet

    Private randStack As RandStack
    Private manaSourcesTypes() As String = New String() {"G000CR0000GR", "G000CR0000RG", "G000CR0000WH", "G000CR0000RD", "G000CR0000YE"}

    Private spells, excludedSpells As New Dictionary(Of String, AllDataStructues.Spell)

    Delegate Function getSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)

    ''' <param name="RStack">Инициализированный класс</param>
    ''' <param name="AllSpells">Dсе заклинания в игре</param>
    Public Sub New(ByRef RStack As RandStack, ByRef AllSpells() As AllDataStructues.Spell)

        randStack = RStack

        Dim excludeItems As New List(Of Integer)
        excludeItems.AddRange(New Integer() {10, 14})
        For i As Integer = 0 To UBound(AllSpells) Step 1
            If Not randStack.comm.excludedObjects.Contains(AllSpells(i).spellID.ToUpper) Then
                spells.Add(AllSpells(i).spellID.ToUpper, AllSpells(i))
            Else
                excludedSpells.Add(AllSpells(i).spellID.ToUpper, AllSpells(i))
            End If
        Next i

    End Sub

    Private Sub AddToLog(ByRef log As Log, ByRef LogID As Integer, ByRef Msg As String)
        If LogID > -1 Then
            Call Log.MAdd(LogID, Msg)
        Else
            Call Log.Add(Msg)
        End If
    End Sub

    ''' <summary>Определит тип случайной шахты. Если тип уже определен, то этот тип и вернет</summary>
    ''' <param name="mineObjectName">Название шахты, как его выдал генератор</param>
    Public Function SetMineType(ByVal mineObjectName As String) As String
        If mineObjectName.ToUpper = My.Resources.mineTypeRandomMana.ToUpper Then
            Dim r As Integer = randStack.comm.rndgen.RndPos(manaSourcesTypes.Length, True) - 1
            Return manaSourcesTypes(r)
        Else
            Return mineObjectName
        End If
    End Function

    Private Function FindSpell(ByRef id As String) As AllDataStructues.Spell
        Dim u As String = id.ToUpper
        If spells.ContainsKey(u) Then Return spells.Item(u)
        If excludedSpells.ContainsKey(u) Then Return excludedSpells.Item(u)
        Return Nothing
    End Function

    ''' <summary>Создаст список заклинаний</summary>
    ''' <param name="d">Желаемые параметры доступных заклинаний. Имеет значение только .shopContent</param>
    ''' <param name="AllManaSources">Список источников маны на карте (Только такие ID и в верхнем регистре: G000CR0000GR, G000CR0000RG, G000CR0000WH, G000CR0000RD, G000CR0000YE)</param>
    ''' <param name="log">Лог для записей результатов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function MakeSpellsList(ByRef d As AllDataStructues.DesiredStats, ByRef AllManaSources As List(Of String), _
                                   ByRef log As Log,
                                   Optional ByVal LogID As Integer = -1) As List(Of String)

        Call AddToLog(log, LogID, "----Spells creation started----")

        Dim availMana As New AllDataStructues.Cost
        If AllManaSources.Contains("G000CR0000GR") Then availMana.Green = 1
        If AllManaSources.Contains("G000CR0000RG") Then availMana.Black = 1
        If AllManaSources.Contains("G000CR0000WH") Then availMana.White = 1
        If AllManaSources.Contains("G000CR0000RD") Then availMana.Red = 1
        If AllManaSources.Contains("G000CR0000YE") Then availMana.Blue = 1
        Dim races() As String = New String() {"H", "C", "L", "U", "E", "R"}

        Dim res , slist As New List(Of String)
        Dim txt As String

        For Each L As String In d.shopContent
            txt = "In: " & L & " -> "
            If L.Length = 10 AndAlso (L.Substring(0, 1).ToUpper = "G" And L.Substring(4, 2).ToUpper = "SS") Then
                If Not res.Contains(L.ToUpper) Then
                    res.Add(L.ToUpper)
                    txt &= L.ToUpper
                Else
                    txt = ""
                End If
            ElseIf IsNumeric(L) Then
                slist.Clear()
                Dim type As Integer = CInt(L)
                For Each s As AllDataStructues.Spell In spells.Values
                    If Not res.Contains(s.spellID) AndAlso AddSpell(-1, -1, True, availMana, False, s) Then
                        If type > -1 Then
                            If s.category = type Then slist.Add(s.spellID)
                        Else
                            slist.Add(s.spellID)
                        End If
                    End If
                Next s
                Dim selected As String
                If slist.Count > 0 Then
                    selected = slist.Item(randStack.comm.rndgen.RndPos(slist.Count, True) - 1)
                Else
                    selected = ""
                End If
                If Not selected = "" Then res.Add(selected)
                txt &= msgToLog(selected)
            ElseIf L.Contains("#") Then
                Dim s() As String = L.Split(CChar("#"))
                Dim type As Integer = CInt(s(0))
                Dim prop As String = s(1)
                Dim selected As String = SelectSpell(prop, type, races, availMana, res)
                If Not selected = "" Then res.Add(selected)
                txt &= msgToLog(selected)
            Else
                Dim selected As String = SelectSpell(L, -1, races, availMana, res)
                If Not selected = "" Then res.Add(selected)
                txt &= msgToLog(selected)
            End If
            If Not txt = "" Then Call AddToLog(log, LogID, txt)
        Next L

        Call AddToLog(log, LogID, "----Spells creation ended----")

        Return res
    End Function
    Private Function msgToLog(ByRef spellID As String) As String
        If Not spellID = "" Then
            Dim txt As String = spells.Item(spellID).spellID & " - " & _
                                spells.Item(spellID).name & " - " & _
                                "Lvl: " & spells.Item(spellID).level & " - " & _
                                "Cat: " & spells.Item(spellID).category
            Return txt
        Else
            Return "nothing"
        End If
    End Function
    Private Function SelectSpell(ByRef sProperties As String, ByRef type As Integer, ByRef races() As String, _
                                 ByRef availMana As AllDataStructues.Cost, ByRef res As List(Of String)) As String

        Dim level, race As Integer
        Dim mass, ignoreAvailMana As Boolean
        Dim slist As New List(Of String)
        Dim rlist() As Integer

        Dim L As String = sProperties.ToUpper
        If L.Contains("T") Then
            mass = True
            L = L.Replace("T", "")
        Else
            mass = False
            L = L.Replace("F", "")
        End If
        race = -2
        For Each r As String In races
            If L.Contains(r) Then
                If r = "R" Then
                    race = -1
                Else
                    race = randStack.comm.RaceIdentifierToSubrace(r)
                End If
                L = L.Replace(r, "")
                Exit For
            End If
        Next r
        level = CInt(L)
        ignoreAvailMana = False
        rlist = New Integer() {race, -1}
        For rr As Integer = 0 To UBound(rlist) Step 1
            For p As Integer = 0 To 1 Step 1
                For spellLevel As Integer = level To 0 Step -1
                    For Each s As AllDataStructues.Spell In spells.Values
                        If Not res.Contains(s.spellID) AndAlso AddSpell(spellLevel, rlist(rr), mass, _
                                                                     availMana, ignoreAvailMana, s) Then
                            If type > -1 Then
                                If s.category = type Then slist.Add(s.spellID)
                            Else
                                slist.Add(s.spellID)
                            End If
                        End If
                    Next s
                    If slist.Count > 0 Then Exit For
                Next spellLevel
                ignoreAvailMana = Not ignoreAvailMana
                If slist.Count > 0 Then Exit For
            Next p
            If slist.Count > 0 Then Exit For
        Next rr
        If slist.Count > 0 Then
            Return slist.Item(randStack.comm.rndgen.RndPos(slist.Count, True) - 1)
        Else
            Return ""
        End If
    End Function
    Private Function AddSpell(ByRef level As Integer, ByRef race As Integer, ByRef mass As Boolean, _
                              ByRef avail As AllDataStructues.Cost, ByRef ignoreAvail As Boolean, ByRef s As AllDataStructues.Spell) As Boolean
        If Not mass And s.area > 998 Then Return False
        If level > 0 And Not s.level = level Then Return False

        If Not ignoreAvail Then
            If s.castCost.Black > 0 And avail.Black = 0 Then Return False
            If s.castCost.Blue > 0 And avail.Blue = 0 Then Return False
            If s.castCost.Green > 0 And avail.Green = 0 Then Return False
            If s.castCost.Red > 0 And avail.Red = 0 Then Return False
            If s.castCost.White > 0 And avail.White = 0 Then Return False
        End If

        If race > -1 Then
            If IsNothing(s.researchCost) Then Return False
            Dim ok As Boolean = False
            For Each lord As String In s.researchCost.Keys
                If randStack.comm.LordsRace(lord) = race Then
                    ok = True
                    Exit For
                End If
            Next lord
            If Not ok Then Return False
        End If
        Return True
    End Function

    ''' <summary>Создаст список наемников</summary>
    ''' <param name="d">Желаемые параметры доступных наемников. Имеет значение только .shopContent</param>
    ''' <param name="log">Лог для записей результатов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function MakeMercenariesList(ByRef d As AllDataStructues.DesiredStats, _
                                        ByRef log As Log,
                                        Optional ByVal LogID As Integer = -1) As List(Of String)

        Call AddToLog(log, LogID, "----Mercenaries creation started----")

        Dim res As New List(Of String)
        Dim selection As New List(Of Integer)
        Dim txt As String = ""

        For Each v As String In d.shopContent
            txt = "In: " & v & " -> "
            If IsNumeric(v) Then
                Dim bar As Integer = CInt(v)
                Dim selected As Integer = SelectMercenary(bar, -1, res)
                If selected > -1 Then res.Add(randStack.AllFighters(selected).unitID)
                txt &= msgToLog(selected)
            ElseIf randStack.comm.RaceIdentifierToSubrace(v, False) > -1 Then
                selection.Clear()
                Dim selected As Integer = -1
                Dim race As Integer = randStack.comm.RaceIdentifierToSubrace(v)
                For u As Integer = 0 To UBound(randStack.AllFighters) Step 1
                    If Not res.Contains(randStack.AllFighters(u).unitID) Then
                        If race = randStack.AllFighters(u).race Then selection.Add(u)
                    End If
                Next u
                If selection.Count > 0 Then
                    selected = selection.Item(randStack.comm.rndgen.RndPos(selection.Count, True) - 1)
                    res.Add(randStack.AllFighters(selected).unitID)
                End If
                txt &= msgToLog(selected)
            ElseIf v.Contains("#") Then
                Dim s() As String = v.Split(CChar("#"))
                Dim race As Integer
                If IsNumeric(s(0)) Then
                    race = CInt(s(0))
                Else
                    race = randStack.comm.RaceIdentifierToSubrace(s(0))
                End If
                Dim bar As Integer = CInt(s(1))
                Dim selected As Integer = SelectMercenary(bar, race, res)
                If selected > -1 Then res.Add(randStack.AllFighters(selected).unitID)
                txt &= msgToLog(selected)
            Else
                If Not res.Contains(v.ToUpper) Then
                    res.Add(v)
                    txt &= v.ToUpper
                Else
                    txt = ""
                End If
            End If
            If Not txt = "" Then Call AddToLog(log, LogID, txt)
        Next v

        Call AddToLog(log, LogID, "----Mercenaries creation ended----")

        Return res
    End Function
    Private Function msgToLog(ByRef unitID As Integer) As String
        If unitID > 0 Then
            Dim unit As AllDataStructues.Unit = randStack.AllFighters(unitID)
            Dim txt As String = unit.unitID & " - " & unit.name & " - " & randStack.comm.RaceNumberToRaceChar(unit.race) & " - " & unit.EXPnext
            If unit.small Then
                txt &= " (small)"
            Else
                txt &= " (big)"
            End If
            Return txt
        Else
            Return "nothing"
        End If
    End Function
    Private Function SelectMercenary(ByRef bar As Integer, ByRef race As Integer, ByRef added As List(Of String)) As Integer

        Dim selection As New List(Of Integer)

        Dim tolerance As Integer
        Dim dtolerance As Integer = 50
        Dim oneMore As Boolean = False
        Dim add As Boolean

        Do While (selection.Count = 0 Or oneMore) And tolerance <= 10000
            tolerance += dtolerance
            For u As Integer = 0 To UBound(randStack.AllFighters) Step 1
                If Not added.Contains(randStack.AllFighters(u).unitID) Then
                    add = False
                    If randStack.AllFighters(u).small Then
                        If Math.Abs(randStack.AllFighters(u).EXPnext - bar) <= tolerance Then add = True
                    Else
                        If Math.Abs(randStack.AllFighters(u).EXPnext - 2 * bar) <= 2 * tolerance Then add = True
                    End If
                    If add And race > -1 Then
                        If Not race = randStack.AllFighters(u).race Then add = False
                    End If
                    If add Then selection.Add(u)
                End If
            Next u
            If selection.Count > 0 Then oneMore = Not oneMore
        Loop
        'If selection.Count = 0 Then Throw New Exception("Не могу выбрать юнита в качестве наемника. Планка опыта: " & bar.ToString)
        If selection.Count > 0 Then
            Dim r As Integer = selection.Item(randStack.comm.rndgen.RndPos(selection.Count, True) - 1)
            Return r
        Else
            Return -1
        End If
    End Function

    ''' <summary>Создаст список предметов</summary>
    ''' <param name="d">Желаемые параметры доступных предметов. Имеет значение только .shopContent и .IGen.
    ''' IGen используется только при генерации по цене</param>
    ''' <param name="log">Лог для записей результатов</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function MakeMerchantItemsList(ByRef d As AllDataStructues.DesiredStats, _
                                          ByRef log As Log, _
                                          Optional ByVal LogID As Integer = -1) As List(Of String)

        Call AddToLog(log, LogID, "----Alternative loot creation started----")
        Dim txt As String
        Dim res, selection As New List(Of String)
        Dim dCost As Integer = 0
        Dim itemID As Integer = -1
        For Each v As String In d.shopContent
            txt = "In: " & v
            If IsNumeric(v) Then
                Dim bar As Integer = CInt(v)
                itemID = SelectItem(bar, -1, dCost, d, res.Count)
                If itemID > -1 Then res.Add(randStack.MagicItem(itemID).itemID)
                txt &= msgToLog(itemID, dCost, True)
            ElseIf randStack.comm.itemTypeID.ContainsKey(v.ToUpper) Then
                selection.Clear()
                Dim type As Integer = randStack.comm.itemTypeID.Item(v.ToUpper)
                For Each u As AllDataStructues.Item In randStack.MagicItem
                    If type = u.type AndAlso randStack.ItemFilter(d.IGen, u) Then selection.Add(u.itemID)
                Next u
                If selection.Count = 0 Then Throw New Exception("Не могу выбрать предмет в качестве товара. Тип: " & v)
                itemID = randStack.comm.rndgen.RndPos(selection.Count, True) - 1
                res.Add(selection.Item(itemID))
                txt &= msgToLog(itemID, dCost, False)
            ElseIf v.Contains("#") Then
                Dim s() As String = v.Split(CChar("#"))
                Dim type As Integer
                If IsNumeric(s(0)) Then
                    type = CInt(s(0))
                Else
                    type = randStack.comm.itemTypeID.Item(s(0).ToUpper)
                End If
                Dim bar As Integer = CInt(s(1))
                itemID = SelectItem(bar, type, dCost, d, res.Count)
                If itemID > -1 Then res.Add(randStack.MagicItem(itemID).itemID)
                txt &= msgToLog(itemID, dCost, True)
            Else
                res.Add(v.ToUpper)
                txt &= " -> " & v.ToUpper
            End If
            Call AddToLog(log, LogID, txt)
        Next v

        Call AddToLog(log, LogID, "----Alternative loot creation ended----")

        Return res
    End Function
    Private Function msgToLog(ByRef itemID As Integer, ByRef dCost As Integer, ByRef addDeltaCost As Boolean) As String
        Dim txt As String = ""
        If addDeltaCost Then txt &= " deltaCost: " & dCost
        txt &= " -> "
        If itemID > -1 Then
            txt &= randStack.MagicItem(itemID).itemID & " - " & randStack.MagicItem(itemID).name & " - " & randStack.MagicItem(itemID).itemCost.Gold
        Else
            txt &= "nothong"
        End If
        Return txt
    End Function
    Private Function SelectItem(ByRef bar As Integer, ByRef type As Integer, ByRef dCost As Integer, _
                                ByRef d As AllDataStructues.DesiredStats, ByRef addedCount As Integer) As Integer
        Dim selection As New List(Of Integer)
        Dim correctedBar As Integer = bar + CInt(dCost / Math.Max(d.shopContent.Count - addedCount, 1))
        If correctedBar <= 0 Then
            dCost += bar
            Return -1
        End If
        Dim dtolerance As Integer = 100
        Dim tolerance As Integer
        Dim oneMore As Boolean = False
        Dim add As Boolean

        tolerance = 0
        Do While (selection.Count = 0 Or oneMore) And tolerance <= 10000
            tolerance += dtolerance
            selection.Clear()
            For u As Integer = 0 To UBound(randStack.MagicItem) Step 1
                If Math.Abs(randStack.LootCost(randStack.MagicItem(u)).Gold - correctedBar) <= tolerance Then
                    If type < 0 Then
                        add = randStack.ItemFilter(d.IGen, randStack.MagicItem(u))
                    Else
                        If type = randStack.MagicItem(u).type Then
                            add = True
                        Else
                            add = False
                        End If
                    End If
                    If add Then selection.Add(u)
                End If
            Next u
            If selection.Count > 0 Then oneMore = Not oneMore
        Loop
        dCost += bar
        If selection.Count > 0 Then
            Dim r As Integer = selection.Item(randStack.comm.rndgen.RndPos(selection.Count, True) - 1)
            dCost -= randStack.MagicItem(r).itemCost.Gold
            Return r
        Else
            'Throw New Exception("Не могу выбрать предмет в качестве товара. Планка цены: " & bar.ToString)
            Return -1
        End If
    End Function

    Private Function GetSettingsCommon(ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        For Each item In input
            output.Add(item.ToUpper)
        Next item
        Return output
    End Function
    Private Function GetRandomMode(ByRef input As List(Of String), ByRef weight() As Double, ByRef handler As getSettings) As List(Of String)
        Dim output As New List(Of String)
        Dim ID As New List(Of Integer)
        For i As Integer = 0 To UBound(weight) Step 1
            ID.Add(i)
        Next i
        For Each item In input
            Dim r As Integer = randStack.comm.RandomSelection(ID, weight, True) + 1
            output.Add(handler(r, {item}).Item(0))
        Next item
        Return output
    End Function

    ''' <summary>Вернет настройки генерации заклинаний</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - УровеньРасаМассовость, 3 - Тип, 4 - Тип#УровеньРасаМассовость</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetSpellsListSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)
        Dim L As New List(Of String)
        L.AddRange(input)
        Return GetSpellsListSettings(mode, L)
    End Function
    ''' <summary>Вернет настройки генерации заклинаний</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - УровеньРасаМассовость, 3 - Тип, 4 - Тип#УровеньРасаМассовость</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetSpellsListSettings(ByVal mode As Integer, ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        Dim spell As AllDataStructues.Spell
        If mode = -1 Then
            output = GetRandomMode(input, {0.25, 1, 1, 1}, AddressOf GetSpellsListSettings)
        ElseIf mode = 1 Then
            output = GetSettingsCommon(input)
        ElseIf mode = 2 Then
            For Each item In input
                spell = FindSpell(item)
                If Not spell.spellID = "" Then
                    Dim prop As String = SpellProperties(spell)
                    output.Add(prop)
                End If
            Next item
        ElseIf mode = 3 Then
            For Each item In input
                spell = FindSpell(item)
                If Not spell.spellID = "" Then
                    output.Add(spell.category.ToString)
                End If
            Next item
        ElseIf mode = 4 Then
            For Each item In input
                spell = FindSpell(item)
                If Not spell.spellID = "" Then
                    Dim prop As String = SpellProperties(spell)
                    output.Add(spell.category.ToString & "#" & prop)
                End If
            Next item
        Else
            Throw New Exception("Unknown mode: " & mode)
            output = Nothing
        End If
        Return output
    End Function
    Private Function SpellProperties(ByRef spell As AllDataStructues.Spell) As String
        Dim race As String
        If IsNothing(spell.researchCost) OrElse spell.researchCost.Count = 0 Then
            race = "R"
        Else
            Dim lord As String = spell.researchCost.Keys(0)
            race = randStack.comm.RaceNumberToRaceChar.Item(randStack.comm.LordsRace.Item(lord))
        End If
        Dim mass As String
        If spell.area > 998 Then
            mass = "T"
        Else
            mass = "F"
        End If
        Return spell.level & race & mass
    End Function

    ''' <summary>Вернет настройки генерации предметов</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Цена, 3 - Тип, 4 - Тип#Цена</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetMerchantListSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)
        Dim L As New List(Of String)
        L.AddRange(input)
        Return GetMerchantListSettings(mode, L)
    End Function
    ''' <summary>Вернет настройки генерации предметов</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Цена, 3 - Тип, 4 - Тип#Цена</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetMerchantListSettings(ByVal mode As Integer, ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        Dim thing As AllDataStructues.Item
        If mode = -1 Then
            output = GetRandomMode(input, {0.25, 1, 1, 1}, AddressOf GetMerchantListSettings)
        ElseIf mode = 1 Then
            output = GetSettingsCommon(input)
        ElseIf mode = 2 Then
            For Each item In input
                thing = randStack.FindItemStats(item)
                output.Add(AllDataStructues.Cost.Sum(thing.itemCost).ToString)
            Next item
        ElseIf mode = 3 Then
            For Each item In input
                thing = randStack.FindItemStats(item)
                output.Add(randStack.comm.itemType.Item(thing.type))
            Next item
        ElseIf mode = 4 Then
            For Each item In input
                thing = randStack.FindItemStats(item)
                output.Add(randStack.comm.itemType.Item(thing.type) & _
                           "#" & AllDataStructues.Cost.Sum(thing.itemCost))
            Next item
        Else
            Throw New Exception("Unknown mode: " & mode)
            output = Nothing
        End If
        Return output
    End Function

    ''' <summary>Вернет настройки генерации предметов</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Планка опыта, 3 - Раса, 4 - Раса#Планка опыта</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetMercenariesListSettings(ByVal mode As Integer, ByRef input() As String) As List(Of String)
        Dim L As New List(Of String)
        L.AddRange(input)
        Return GetMercenariesListSettings(mode, L)
    End Function
    ''' <summary>Вернет настройки генерации предметов</summary>
    ''' <param name="mode">-1 - случайный мод в каждой строчке, 1 - вернет ID, 2 - Планка опыта, 3 - Раса, 4 - Раса#Планка опыта</param>
    ''' <param name="input">ID заклинаний</param>
    Public Function GetMercenariesListSettings(ByVal mode As Integer, ByRef input As List(Of String)) As List(Of String)
        Dim output As New List(Of String)
        Dim unit As AllDataStructues.Unit
        If mode = -1 Then
            output = GetRandomMode(input, {0.25, 1, 1, 1}, AddressOf GetMercenariesListSettings)
        ElseIf mode = 1 Then
            output = GetSettingsCommon(input)
        ElseIf mode = 2 Then
            For Each item In input
                unit = randStack.FindUnitStats(item)
                Dim expNext As Integer = unit.EXPnext
                If Not unit.small Then expNext = CInt(expNext * 0.5)
                output.Add(expNext.ToString)
            Next item
        ElseIf mode = 3 Then
            For Each item In input
                unit = randStack.FindUnitStats(item)
                output.Add(randStack.comm.RaceNumberToRaceChar(unit.race))
            Next item
        ElseIf mode = 4 Then
            For Each item In input
                unit = randStack.FindUnitStats(item)
                Dim expNext As Integer = unit.EXPnext
                If Not unit.small Then expNext = CInt(expNext * 0.5)
                output.Add(randStack.comm.RaceNumberToRaceChar(unit.race) & _
                           "#" & expNext.ToString)
            Next item
        Else
            Throw New Exception("Unknown mode: " & mode)
            output = Nothing
        End If
        Return output
    End Function

End Class