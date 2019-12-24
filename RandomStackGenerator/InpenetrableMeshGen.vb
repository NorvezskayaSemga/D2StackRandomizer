Imports System.Drawing
Imports System.ComponentModel
Imports System.Threading.Tasks

Public Class InpenetrableMeshGen

    Public Structure Cell
        ''' <summary>ID локаций, с которыми связана эта клетка</summary>
        Dim locID As List(Of Integer)
        ''' <summary>True, если на клетке должен стоять непроходимый непосещаемый объект</summary>
        Dim isBorder As Boolean
        ''' <summary>True, если клетка является частью прохода между локациями</summary>
        Dim isPass As Boolean
        ''' <summary>True, если клетка находится под посещаемым объектом</summary>
        Dim isAttended As Boolean
        ''' <summary>Если клетка является верхним левым углом посещаемого объекта, то здесь хранится ID объекта</summary>
        Dim objectID As Integer
    End Structure

    Public Structure Map
        ''' <summary>Список локаций. Первые в списке - стартовые по числу играбельных рас на карте</summary>
        Dim Loc As Location()
        ''' <summary>Поле карты, содержащее свойства каждой клетки</summary>
        Dim board(,) As Cell
        ''' <summary>Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 23)</summary>
        Dim xSize As Integer
        ''' <summary>Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 47)</summary>
        Dim ySize As Integer
    End Structure

    Private Structure PrepareToRaceLocGenResult
        Dim DminimumDist As Double
        ''' <summary>Минимальное расстояние^2</summary>
        Dim IminimumDist As Integer
        Dim possiblePoints() As Point
        Dim ppIDs As List(Of Integer)
        Dim raceLocs() As Location
        Dim equalDist(,) As List(Of Integer)
    End Structure

    Public Structure SettingsLoc
        ''' <summary>Средний радиус локаций</summary>
        Dim AverageRadius As Integer
        ''' <summary>Локации будут в форме эллипсов со случайным эксцентриситетом от (1-D)/(1+D) до (1+D)/(1-D)</summary>
        Dim maxEccentricityDispersion As Double
        ''' <summary>При достаточном количестве места будут создаваться локации размером от (1-D)*R до (1+D)*R.
        ''' Когда свободного места станет недостаточно R начнет постепенно уменьшаться до половины от начального значения</summary>
        Dim maxRadiusDispersion As Double

        '''<summary>Максимальное количество золотых шахт на локацию</summary>
        Dim maxGoldMines As Integer
        '''<summary>Максимальное количество источников маны на локацию</summary>
        Dim maxManaSources As Integer
    End Structure
    Public Structure SettingsMap
        ''' <summary>Правая граница карты (например, если генерируем карту 24x48, то сюда пишем 23)</summary>
        Dim xSize As Integer
        ''' <summary>Верхняя граница карты (например, если генерируем карту 24x48, то сюда пишем 47)</summary>
        Dim ySize As Integer
        ''' <summary>Минимальная ширина проходов</summary>
        Dim minPassDist As Double
        ''' <summary>Минимальное расстояние между проходами</summary>
        Dim minPassWidth As Double
        ''' <summary>Количество рас</summary>
        Dim nRaces As Integer
        ''' <summary>Генератор будет располагать локации со столицами так, чтобы для каждой из локаций выполнялось следующиее условие:
        ''' R1*(1+T) >= R2, при этом R2 > R1, где R1 и R2 - расстояние до двух ближайших локаций</summary>
        Dim RaceLocsDistTolerance As Double
    End Structure

    Public Structure AttendedObject
        Dim TypeID As Integer
        Dim Size As Integer
        Dim Name As String
        Dim Subtype As Integer
        Dim needExpand As Boolean
    End Structure

    Private rndgen As New RndValueGen
    Private comm As New Common
    Private symm As New SymmetryOperations
    Public ActiveObjects() As AttendedObject = New AttendedObject() {Nothing, _
                        New AttendedObject With {.Name = "Capital", .needExpand = True, .Size = 5, .TypeID = 1}, _
                        New AttendedObject With {.Name = "City", .needExpand = True, .Size = 4, .TypeID = 2}, _
                        New AttendedObject With {.Name = "Vendor", .needExpand = True, .Size = 3, .TypeID = 3}, _
                        New AttendedObject With {.Name = "Mercenary", .needExpand = True, .Size = 3, .TypeID = 4}, _
                        New AttendedObject With {.Name = "Mage", .needExpand = True, .Size = 3, .TypeID = 5}, _
                        New AttendedObject With {.Name = "Trainer", .needExpand = True, .Size = 3, .TypeID = 6}, _
                        New AttendedObject With {.Name = "Ruins", .needExpand = True, .Size = 3, .TypeID = 7}, _
                        New AttendedObject With {.Name = "Mine", .needExpand = False, .Size = 3, .TypeID = 8}}


    ''' <summary>Генерирует заготовку ландшафта без использования симметрии</summary>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций</param>
    Public Function UnsymmGen(ByRef settMap As SettingsMap, ByRef settRaceLoc As SettingsLoc, ByRef settCommLoc As SettingsLoc) As Map

        Dim t0 As Integer = Environment.TickCount
        Dim m As Map = UnsymmPlaceRaceLocations(settMap, settRaceLoc)
        Dim t1 As Integer = Environment.TickCount
        Call UnsymmPlaceCommonLocs(m, settMap, settCommLoc)
        Dim t2 As Integer = Environment.TickCount
        Call UnsymmSetLocIdToCells(m)
        Dim t3 As Integer = Environment.TickCount
        Call SetBorders(m, settMap)
        Dim t4 As Integer = Environment.TickCount
        Call PlaceActiveObjects(m, settMap, settRaceLoc, settCommLoc, -1)
        Dim t5 As Integer = Environment.TickCount
        Console.WriteLine("RLocs: " & t1 - t0 & vbTab & "CLocs: " & t2 - t1 & vbTab & "IDset: " & t3 - t2 & vbTab & "BordSet: " & t4 - t3 & vbTab & "PlaceActive: " & t5 - t4)
        Return m
    End Function
    ''' <summary>Генерирует заготовку ландшафта с использованием симметрии</summary>
    ''' <param name="settMap">Общие настройки для карты</param>
    ''' <param name="settRaceLoc">Настройки для стартовых локаций играбельных рас</param>
    ''' <param name="settCommLoc">Настройки для остальных локаций</param>
    ''' <param name="symmID">ID применяемой операпции симметрии (см. класс SymmetryOperations).
    ''' Если ID меньше ноля, будет выбрана случайная симметрия из тех, что подходят</param>
    Public Function SymmGen(ByRef settMap As SettingsMap, ByRef settRaceLoc As SettingsLoc, _
                            ByRef settCommLoc As SettingsLoc, Optional ByRef symmID As Integer = -1) As Map
        Dim s As Integer
        Dim slist As List(Of Integer) = symm.PossibleOperationsList(settMap.nRaces, _
                               New Map With {.xSize = settMap.xSize, .ySize = settMap.ySize})
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

        Dim t0 As Integer = Environment.TickCount
        Dim m As Map = SymmPlaceRaceLocations(settMap, settRaceLoc, s)
        Dim t1 As Integer = Environment.TickCount
        Call SymmPlaceCommonLocs(m, settMap, settCommLoc, s)
        Dim t2 As Integer = Environment.TickCount
        Call SymmSetLocIdToCells(m, settMap, s)
        Dim t3 As Integer = Environment.TickCount
        Call SetBorders(m, settMap, s)
        Dim t4 As Integer = Environment.TickCount
        Call PlaceActiveObjects(m, settMap, settRaceLoc, settCommLoc, s)
        Dim t5 As Integer = Environment.TickCount
        Console.WriteLine("RLocs: " & t1 - t0 & vbTab & "CLocs: " & t2 - t1 & vbTab & "IDset: " & t3 - t2 & vbTab & "BordSet: " & t4 - t3 & vbTab & "PlaceActive: " & t5 - t4)
        Return m
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
    Private Function NearestXY(ByRef x As Integer, ByRef y As Integer, _
                               ByRef xSize As Integer, ByRef ySize As Integer, _
                               ByRef tolerance As Integer) As Location.Borders
        Return New Location.Borders With {.minx = Math.Max(x - tolerance, 0), _
                                          .maxx = Math.Min(x + tolerance, xSize), _
                                          .miny = Math.Max(y - tolerance, 0), _
                                          .maxy = Math.Min(y + tolerance, ySize)}
    End Function
    Private Function NearestXY(ByRef P As Point, ByRef M As Map, ByRef tolerance As Integer) As Location.Borders
        Return NearestXY(P.X, P.Y, M.xSize, M.ySize, tolerance)
    End Function

    Private Function UnsymmPlaceRaceLocations(ByRef settMap As SettingsMap, ByRef settRaceLoc As SettingsLoc) As Map

        Dim res As New Map With {.xSize = settMap.xSize, .ySize = settMap.ySize}
        Dim ok As Boolean = False
        Dim tryagain As Boolean = False
        Dim borderPoints() As Point = Nothing
        Dim id, t As Integer

        Dim prepResult As PrepareToRaceLocGenResult = PrepareToRaceLocGen(settMap, settRaceLoc)
        Dim raceLocs() As Location = prepResult.raceLocs
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
        res.Loc = raceLocs
        Call ResetBoard(res)
        For i As Integer = 0 To UBound(res.Loc) Step 1
            Call PlaceLoc(res, res.Loc(i))
        Next i
        Return res
    End Function
    Private Function SymmPlaceRaceLocations(ByRef settMap As SettingsMap, ByRef settRaceLoc As SettingsLoc, _
                                            ByRef symmID As Integer) As Map

        Dim res As New Map With {.xSize = settMap.xSize, .ySize = settMap.ySize}
        Dim ok As Boolean = False

        Dim id As Integer
        Dim posList(), L As Location
        Dim prepResult As PrepareToRaceLocGenResult = PrepareToRaceLocGen(settMap, settRaceLoc)
        Dim raceLocs() As Location = prepResult.raceLocs
        posList = Nothing
        Do While Not ok
            L = prepResult.raceLocs(0).Copy
            id = comm.RandomSelection(prepResult.ppIDs, True)
            L.pos = New Point(prepResult.possiblePoints(id).X, prepResult.possiblePoints(id).Y)
            posList = symm.ApplySymm(L, settMap.nRaces, res, symmID, prepResult.IminimumDist)
            If posList.Length = settMap.nRaces Then
                ok = TestRaceLocations(posList, settMap.nRaces, prepResult, settMap.RaceLocsDistTolerance)
            Else
                ok = False
            End If
        Loop
        res.Loc = posList
        Call ResetBoard(res)
        For i As Integer = 0 To UBound(res.Loc) Step 1
            Call PlaceLoc(res, res.Loc(i))
        Next i
        Return res
    End Function
    Private Function PrepareToRaceLocGen(ByRef settMap As SettingsMap, ByRef settRaceLoc As SettingsLoc) As PrepareToRaceLocGenResult
        Dim result As New PrepareToRaceLocGenResult
        ReDim result.raceLocs(settMap.nRaces - 1)
        For i As Integer = 0 To settMap.nRaces - 1 Step 1
            result.raceLocs(i) = GenLocSize(settRaceLoc, i + 1)
        Next i

        If Math.Max(settMap.xSize, settMap.ySize) < 2 * settRaceLoc.AverageRadius + 10 Then Throw New Exception("Слишком большой радиус начальных локаций")
        If settRaceLoc.AverageRadius < 7 Then Throw New Exception("Слишком маленький радиус начальных локаций")

        Dim r, m As Double
        Dim n As Integer
        If settMap.nRaces = 4 Then
            r = 0.45 * Math.Sqrt(((settMap.xSize - 2 * settRaceLoc.AverageRadius) ^ 2) _
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
                 k += SqDist(result.possiblePoints(i), borderPoints(j))
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
                     d = SqDist(result.possiblePoints(i), result.possiblePoints(j))
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
                    D = SqDist(RLocs(i).pos, RLocs(j).pos)
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

    Private Sub UnsymmPlaceCommonLocs(ByRef m As Map, ByRef settMap As SettingsMap, ByRef settCommLoc As SettingsLoc)
        Dim id As Integer = m.Loc.Length + 1
        Dim dynRadiusDispersion As Double = settCommLoc.maxRadiusDispersion
        Dim dynAverageRadius As Double = settCommLoc.AverageRadius
        Dim dynBaseRadius As Double = settCommLoc.AverageRadius
        Dim possiblePoints((m.xSize + 1) * (m.ySize + 1) - 1) As Point
        Dim IDs As List(Of Integer)
        Dim nextloop As Boolean = True
        Do While nextloop
            Dim loc As Location = GenLocSize(settCommLoc, id)
            IDs = MakePossiblePosList(m, loc, possiblePoints)
            If IDs.Count > 0 Then
                Dim pid As Integer = comm.RandomSelection(IDs, True)
                loc.pos = New Point(possiblePoints(pid).X, possiblePoints(pid).Y)
                ReDim Preserve m.Loc(m.Loc.Length)
                m.Loc(UBound(m.Loc)) = loc
                Call PlaceLoc(m, m.Loc(UBound(m.Loc)))
                id += 1
            Else
                nextloop = ChangeDynamicParameters(dynRadiusDispersion, dynBaseRadius, dynAverageRadius, settCommLoc)
            End If
        Loop
    End Sub
    Private Sub SymmPlaceCommonLocs(ByRef m As Map, ByRef settMap As SettingsMap, ByRef settCommLoc As SettingsLoc, ByRef symmID As Integer)
        Dim id As Integer = m.Loc.Length + 1
        Dim dynRadiusDispersion As Double = settCommLoc.maxRadiusDispersion
        Dim dynAverageRadius As Double = settCommLoc.AverageRadius
        Dim dynBaseRadius As Double = settCommLoc.AverageRadius
        Dim possiblePoints((m.xSize + 1) * (m.ySize + 1) - 1) As Point
        Dim IDs As List(Of Integer)
        Dim sLocs() As Location
        Dim nextloop As Boolean = True
        Do While nextloop
            Dim loc As Location = GenLocSize(settCommLoc, id)
            IDs = MakePossiblePosList(m, loc, possiblePoints)
            If IDs.Count > 0 Then
                Dim pid As Integer = comm.RandomSelection(IDs, True)
                loc.pos = New Point(possiblePoints(pid).X, possiblePoints(pid).Y)
                Dim minR As Integer = CInt((0.9 * Math.Min(loc.gASize, loc.gBSize)) ^ 2)
                sLocs = symm.ApplySymm(loc, settMap.nRaces, m, symmID, minR)
                ReDim Preserve m.Loc(m.Loc.Length + UBound(sLocs))
                For i As Integer = 0 To UBound(sLocs) Step 1
                    m.Loc(UBound(m.Loc) - UBound(sLocs) + i) = sLocs(i)
                    Call PlaceLoc(m, m.Loc(UBound(m.Loc) - UBound(sLocs) + i))
                Next i
                id = m.Loc.Length + 1
            Else
                nextloop = ChangeDynamicParameters(dynRadiusDispersion, dynBaseRadius, dynAverageRadius, settCommLoc)
            End If
        Loop
    End Sub
    Private Function MakePossiblePosList(ByRef m As Map, ByRef loc As Location, ByRef possiblePoints() As Point) As List(Of Integer)
        Dim b As Location.Borders = loc.XYborders(Integer.MaxValue, Integer.MaxValue, Integer.MinValue, Integer.MinValue)
        Dim add As Boolean
        Dim locPlist As New List(Of Point)
        Dim IDs As New List(Of Integer)

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
        Return IDs
    End Function
    Private Function ChangeDynamicParameters(ByRef dynRadiusDispersion As Double, ByRef dynBaseRadius As Double, _
                                             ByRef dynAverageRadius As Double, ByRef settCommLoc As SettingsLoc) As Boolean
        Dim newDisp As Double = 0.9 * dynRadiusDispersion
        If newDisp < 0.001 Then
            dynRadiusDispersion = settCommLoc.maxRadiusDispersion
            dynBaseRadius *= 0.9
            dynAverageRadius = dynBaseRadius
            If dynBaseRadius < 0.5 * settCommLoc.AverageRadius Then Return False
        Else
            dynAverageRadius *= (1 - dynRadiusDispersion) / (1 - newDisp)
            dynRadiusDispersion = newDisp
        End If
        Return True
    End Function

    Private Sub UnsymmSetLocIdToCells(ByRef m As Map)
        Dim tmpm As Map = m
        Dim allPoints()() As Point = Nothing
        Dim pID()() As Integer = Nothing
        Dim selectedIDs() As Integer = Nothing
        Dim Weight()() As Double = Nothing
        Dim minweight As Double
        Dim selectedWeight() As Double = Nothing
        Dim calculatedWeights(,,) As Double = Nothing
        Dim idlist As List(Of Integer) = Nothing

        Call PrepareToSetLocID(tmpm, allPoints, pID, Weight, minweight, selectedIDs, _
                               selectedWeight, calculatedWeights, idlist)
        Do While idlist.Count > 0
            Call makePointsList(tmpm, idlist, allPoints, pID, Weight, minweight, selectedIDs, selectedWeight, calculatedWeights)
            If idlist.Count > 0 Then
                Dim s As Integer = comm.RandomSelection(idlist, selectedWeight, True)
                tmpm.board(allPoints(s)(selectedIDs(s)).X, _
                        allPoints(s)(selectedIDs(s)).Y).locID.Add(pID(s)(selectedIDs(s)))
            End If
        Loop
        m = tmpm
    End Sub
    Private Sub SymmSetLocIdToCells(ByRef m As Map, ByRef settMap As SettingsMap, ByRef symmID As Integer)
        Dim tmpm As Map = m
        Dim allPoints()() As Point = Nothing
        Dim pID()() As Integer = Nothing
        Dim selectedIDs() As Integer = Nothing
        Dim Weight()() As Double = Nothing
        Dim minweight As Double
        Dim selectedWeight() As Double = Nothing
        Dim calculatedWeights(,,) As Double = Nothing
        Dim idlist As List(Of Integer) = Nothing

        Call PrepareToSetLocID(tmpm, allPoints, pID, Weight, minweight, selectedIDs, _
                               selectedWeight, calculatedWeights, idlist)
        Do While idlist.Count > 0
            Call makePointsList(tmpm, idlist, allPoints, pID, Weight, minweight, selectedIDs, selectedWeight, calculatedWeights)
            If idlist.Count > 0 Then
                Dim s As Integer = comm.RandomSelection(idlist, selectedWeight, True)
                Dim pp() As Point = symm.ApplySymm(allPoints(s)(selectedIDs(s)), settMap.nRaces, tmpm, symmID, 1)
                If pp.Length > 1 Then
                    Dim pl() As Point = symm.ApplySymm(tmpm.Loc(pID(s)(selectedIDs(s)) - 1).pos, settMap.nRaces, tmpm, symmID, 1)
                    If pl.Length = pp.Length Then
                        For i As Integer = 0 To UBound(pl) Step 1
                            tmpm.board(pp(i).X, pp(i).Y).locID.Add(tmpm.Loc(FindLocIDByPosition(tmpm, pl(i))).ID)
                        Next i
                    ElseIf pl.Length = 1 Then
                        For i As Integer = 0 To UBound(pp) Step 1
                            tmpm.board(pp(i).X, pp(i).Y).locID.Add(pID(s)(selectedIDs(s)))
                        Next i
                    Else
                        Dim possibleLocs As New List(Of Integer)
                        Dim usedLocs As New List(Of Integer)
                        For Each p As Point In pp
                            Dim b As Location.Borders = NearestXY(p.X, p.Y, tmpm.xSize, tmpm.ySize, 1)
                            possibleLocs.Clear()
again:
                            For i As Integer = b.minX To b.maxX Step 1
                                For j As Integer = b.minY To b.maxY Step 1
                                    If tmpm.board(i, j).locID.Count > 0 Then
                                        Dim locID As Integer = tmpm.board(i, j).locID.Item(0) - 1
                                        If locID > -1 AndAlso Not usedLocs.Contains(locID) Then
                                            For L As Integer = 0 To UBound(pl) Step 1
                                                If Math.Abs(tmpm.Loc(locID).pos.X - pl(L).X) < 2 _
                                                 And Math.Abs(tmpm.Loc(locID).pos.Y - pl(L).Y) < 2 Then
                                                    possibleLocs.Add(locID)
                                                End If
                                            Next L
                                        End If
                                    End If
                                Next j
                            Next i
                            If possibleLocs.Count = 0 And usedLocs.Count > 0 Then
                                usedLocs.Clear()
                                GoTo again
                            End If
                            If possibleLocs.Count = 0 Then Throw New Exception("Не могу найти подходящую локацию")
                            Dim sel As Integer = comm.RandomSelection(possibleLocs, True)
                            tmpm.board(p.X, p.Y).locID.Add(tmpm.Loc(sel).ID)
                            usedLocs.Add(sel)
                        Next p
                    End If
                Else
                    tmpm.board(pp(0).X, pp(0).Y).locID.Add(pID(s)(selectedIDs(s)))
                End If
            End If
        Loop
        m = tmpm
    End Sub
    Private Sub PrepareToSetLocID(ByRef m As Map, ByRef allPoints()() As Point, ByRef pID()() As Integer, _
                                  ByRef Weight()() As Double, ByRef minweight As Double, _
                                  ByRef selectedIDs() As Integer, ByRef selectedWeight() As Double, _
                                  ByRef calculatedWeights(,,) As Double, ByRef idlist As List(Of Integer))
        Call ResetBoard(m)
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
    Private Function FindLocIDByPosition(ByRef m As Map, ByRef p As Point) As Integer
        For j As Integer = 0 To UBound(m.Loc) Step 1
            If Math.Abs(m.Loc(j).pos.X - p.X) < 2 And Math.Abs(m.Loc(j).pos.Y - p.Y) < 2 Then Return j
            If j = UBound(m.Loc) Then Throw New Exception("Не могу найти локацию по координате")
        Next j
        Return -1
    End Function

    Private Sub SetBorders(ByRef m As Map, ByVal settMap As SettingsMap, Optional ByVal symmID As Integer = -1)
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
                        If symmID > -1 Then
                            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, tmpm, symmID, 1)
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
                        If symmID > -1 Then
                            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, tmpm, symmID, 1)
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

        Dim equalLocPairsList As New List(Of String)
        If symmID > -1 Then
            Dim Id(1)(), a() As Integer
            Dim freezed As New List(Of String)
            For i As Integer = 0 To UBound(tmpm.Loc) - 1 Step 1
                For j As Integer = i + 1 To UBound(tmpm.Loc) Step 1
                    If LocBorders(i, j).Count > 0 Then
                        freezed.Add(i & "_" & j)
                        a = New Integer() {i, j}
                        Parallel.For(0, 2, _
                         Sub(k As Integer)
                             Dim p() As Point = symm.ApplySymm(tmpm.Loc(a(k)).pos, settMap.nRaces, tmpm, symmID, 1)
                             ReDim Id(k)(UBound(p))
                             For q As Integer = 0 To UBound(p) Step 1
                                 Id(k)(q) = FindLocIDByPosition(tmpm, p(q))
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
                             D = SqDist(pointsslist(p1), pointsslist(p2))
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
                             D = SqDist(pointsslist(s), pointsslist(p))
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
                             Call MakePass(tmpm, pointsslist(p), c, settMap, symmID)
                         Next p
                     Next c
                 End If
             Next j
         End Sub)

        Dim conn2(0, 0) As Boolean
        Do While Not IsNothing(conn2)
            conn2 = Nothing
            Dim conn1(,) As Boolean = FindConnected(tmpm, m.Loc(0).pos)
            For y As Integer = 0 To tmpm.ySize Step 1
                For x As Integer = 0 To tmpm.xSize Step 1
                    If Not m.board(x, y).isBorder And Not conn1(x, y) Then
                        conn2 = FindConnected(tmpm, New Point(x, y))
                        Dim minD As Integer = Integer.MaxValue
                        Dim D As Integer
                        Dim p1, p2 As Point
                        For y1 As Integer = 0 To tmpm.ySize Step 1
                            For x1 As Integer = 0 To tmpm.xSize Step 1
                                If conn1(x1, y1) Then
                                    For y2 As Integer = 0 To tmpm.ySize Step 1
                                        For x2 As Integer = 0 To tmpm.xSize Step 1
                                            If conn2(x2, y2) Then
                                                D = SqDist(x1, y1, x, y2)
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
                        Call MakePass(tmpm, p1, p2, settMap, symmID)
                        Exit For
                    End If
                Next x
                If Not IsNothing(conn2) Then Exit For
            Next y
        Loop
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
    Private Sub MakePass(ByRef m As Map, ByRef init As Point, ByRef dest As Point, _
                         ByRef settMap As SettingsMap, ByRef symmID As Integer)
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
                        If dist < 0.5 * settMap.minPassWidth OrElse (dist <= settMap.minPassWidth AndAlso rndgen.Rand(0, 1) > 0.5) Then
                            Dim c1 As Integer = Math.Max(Math.Min(x, m.xSize - 1), 1)
                            Dim c2 As Integer = Math.Max(Math.Min(y, m.ySize - 1), 1)
                            Dim p() As Point
                            If symmID > -1 Then
                                p = symm.ApplySymm(New Point(c1, c2), settMap.nRaces, m, symmID, 1)
                            Else
                                p = New Point() {New Point(c1, c2)}
                            End If
                            For Each item As Point In p
                                m.board(item.X, item.Y).isBorder = False
                                m.board(item.X, item.Y).isPass = True
                            Next item
                        End If
                    End If
                Next y
            Next x
        Next r
    End Sub
    Private Function FindConnected(ByRef m As Map, ByRef init As Point) As Boolean(,)
        Dim connected(m.xSize, m.ySize), check(m.xSize, m.ySize) As Boolean
        check(init.X, init.Y) = True
        connected(init.X, init.Y) = True
        Dim r As Integer = 1
        Do While r > 0
            For j As Integer = 0 To m.ySize Step 1
                For i As Integer = 0 To m.ySize Step 1
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

    Private Sub ResetBoard(ByRef m As Map)
        ReDim m.board(m.xSize, m.ySize)
        For x As Integer = 0 To m.xSize Step 1
            For y As Integer = 0 To m.ySize Step 1
                m.board(x, y).locID = New List(Of Integer)
            Next y
        Next x
    End Sub
    Private Function GenLocSize(ByRef sett As SettingsLoc, ByRef id As Integer) As Location
        Dim r, e, a As Double
        r = rndgen.PRand(1 - sett.maxRadiusDispersion, 1 + sett.maxRadiusDispersion) * sett.AverageRadius
        e = rndgen.PRand(1 - sett.maxEccentricityDispersion, 1 + sett.maxEccentricityDispersion)
        a = rndgen.PRand(0, Math.PI)
        Return New Location(New Point(0, 0), r * e, r / e, a, id)
    End Function
    Private Function SqDist(ByRef p1 As Point, ByRef p2 As Point) As Integer
        Return SqDist(p1.X, p1.Y, p2.X, p2.Y)
    End Function
    Private Function SqDist(ByRef x1 As Integer, ByRef y1 As Integer, ByRef x2 As Integer, ByRef y2 As Integer) As Integer
        Dim dx As Integer = x1 - x2
        Dim dy As Integer = y1 - y2
        Return dx * dx + dy * dy
    End Function

    Private Sub PlaceActiveObjects(ByRef tm As Map, ByVal settMap As SettingsMap, _
                                   ByRef settRaceLoc As SettingsLoc, ByRef settCommLoc As SettingsLoc, _
                                   ByVal symmID As Integer)
        Dim m As Map = tm
        Dim LocsPlacing(UBound(m.Loc)) As Location.Borders
        Dim LocArea(UBound(m.Loc)) As Integer
        Parallel.For(0, m.Loc.Length, _
         Sub(i As Integer)
             Dim id As Integer = m.Loc(i).ID
             LocsPlacing(i) = New Location.Borders With {.minX = Integer.MaxValue, .minY = Integer.MaxValue, _
                                                         .maxX = Integer.MinValue, .maxY = Integer.MinValue}
             For y As Integer = 0 To m.ySize Step 1
                 For x As Integer = 0 To m.xSize Step 1
                     If m.board(x, y).locID.Item(0) = id Then
                         LocsPlacing(i).minX = Math.Min(LocsPlacing(i).minX, x)
                         LocsPlacing(i).minY = Math.Min(LocsPlacing(i).minY, y)
                         LocsPlacing(i).maxX = Math.Max(LocsPlacing(i).maxX, x)
                         LocsPlacing(i).maxY = Math.Max(LocsPlacing(i).maxY, y)
                         If MayPlaceObject(m, 8, x, y) Then LocArea(id - 1) += 1
                     End If
                 Next x
             Next y
         End Sub)

        Dim LocAng() As Double = Nothing
        If symmID < 0 Then
            ReDim LocAng(settMap.nRaces - 1)
            Parallel.For(0, settMap.nRaces, _
             Sub(i As Integer)
                 Dim a As Double = 0
                 Dim da As Double = 0.005
                 Dim minR As Double = Double.MaxValue
                 Dim r As Double
                 Do While a < 2 * Math.PI
                     r = ((CDbl(m.Loc(i).pos.X) - Math.Cos(a) - 0.5 * CDbl(m.xSize)) ^ 2) _
                       + ((CDbl(m.Loc(i).pos.Y) - Math.Sin(a) - 0.5 * CDbl(m.ySize)) ^ 2)
                     If minR > r Then
                         minR = r
                         LocAng(i) = a
                     End If
                     a += da
                 Loop
             End Sub)
        End If

        Dim places(settRaceLoc.maxGoldMines + settRaceLoc.maxManaSources) As Integer
        Dim nearWith(UBound(places)) As Integer
        places(0) = 1
        For i As Integer = 1 To settRaceLoc.maxGoldMines + settRaceLoc.maxManaSources Step 1
            places(i) = 8
            If i < 4 Then
                nearWith(i) = 0
            Else
                nearWith(i) = -1
            End If
        Next i
        Dim ok As Boolean = False
        Do While Not ok
            Dim v()() As Point = Nothing
            If symmID > -1 Then
                ReDim v(0)
                Call ObjectsPlacingVariants(places, 1, m, LocsPlacing, nearWith, v(0))
            Else
                ReDim v(settMap.nRaces - 1)
                Parallel.For(0, settMap.nRaces, _
                 Sub(i As Integer)
                     Call ObjectsPlacingVariants(places, i + 1, m, LocsPlacing, nearWith, v(i))
                 End Sub)
            End If
            nearWith = Nothing
            Dim minN As Integer = Integer.MaxValue
            For i As Integer = 0 To UBound(v) Step 1
                If IsNothing(v(i)) Then Throw New Exception("Как минимум одна из стартовых локаций настолько маленькая, что я не могу разместить даже столицу")
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
                    Rsum += SqDist(v(i)(n), v(i)(0))
                Next n
                minRsum = Math.Min(minRsum, Rsum)
                maxRsum = Math.Max(maxRsum, Rsum)
            Next i
            Dim tolerance As Double = 0.333 * (Math.Pow(1 + settRaceLoc.maxRadiusDispersion, 2) + _
                         Math.Pow(1 + settRaceLoc.maxEccentricityDispersion, 2) + 1.2 ^ 2)
            If minRsum * tolerance < maxRsum Then ok = False
            If ok Then
                Parallel.For(0, v.Length, _
                 Sub(i As Integer)
                     For n As Integer = 0 To minN Step 1
                         Call PlaceObject(m, places(n), v(i)(n).X, v(i)(n).Y, settMap, symmID)
                     Next n
                 End Sub)
            End If
        Loop

        Exit Sub
        Do While True
            Dim baseAngle As Double = rndgen.PRand(0, 2 * Math.PI)
            Dim tLoc() As Location
            If symmID > -1 Then
                tLoc = New Location() {New Location(m.Loc(0).pos, 1, 1, baseAngle, -1)}
            Else
                ReDim tLoc(settMap.nRaces - 1)
                For i As Integer = 0 To settMap.nRaces - 1 Step 1
                    tLoc(i) = New Location(m.Loc(i).pos, 1, 1, baseAngle + LocAng(i), i - settMap.nRaces)
                Next i
            End If




            Exit Do
        Loop

    End Sub
    Private Sub ObjectsPlacingVariants(ByRef objIDs() As Integer, ByRef locID As Integer, _
                                       ByRef m As Map, ByRef LocsPlacing() As Location.Borders, _
                                       ByRef NearWith() As Integer, ByRef output() As Point)
        If IsNothing(NearWith) Then
            ReDim NearWith(UBound(objIDs))
            For i As Integer = 0 To UBound(NearWith) Step 1
                NearWith(i) = -1
            Next i
        End If
        Dim dx As Integer = LocsPlacing(locID - 1).minX
        Dim dy As Integer = LocsPlacing(locID - 1).minY
        Dim freeCells(LocsPlacing(locID - 1).maxX - dx, _
                      LocsPlacing(locID - 1).maxY - dy) As Boolean
        For y As Integer = 0 To UBound(freeCells, 2) Step 1
            For x As Integer = 0 To UBound(freeCells, 1) Step 1
                If Not m.board(x + dx, y + dy).isBorder And Not m.board(x + dx, y + dy).isAttended _
                   AndAlso locID = m.board(x + dx, y + dy).locID.Item(0) Then
                    freeCells(x, y) = True
                End If
            Next x
        Next y
        Dim tmpIDs() As Integer = CType(objIDs.Clone, Integer())
        ReDim output(0)
        Do While output(0).IsEmpty
            ReDim output(UBound(tmpIDs))
            Call PlaceObjRow(tmpIDs, 0, NearWith, freeCells, output)
            If output(0).IsEmpty Then
                If UBound(tmpIDs) > 0 Then
                    ReDim Preserve tmpIDs(UBound(tmpIDs) - 1)
                Else
                    output = Nothing
                    Exit Sub
                End If
            End If
        Loop
        For i As Integer = 0 To UBound(output) Step 1
            output(i) = New Point(output(i).X + dx, output(i).Y + dy)
        Next i
    End Sub
    Private Sub PlaceObjRow(ByRef objIDs() As Integer, ByRef n As Integer, _
                            ByRef NearWith() As Integer, _
                            ByRef freeCells(,) As Boolean, ByRef output() As Point)
        Dim fc_bak(,) As Boolean = CType(freeCells.Clone, Boolean(,))
        Dim fc(,) As Boolean
        Dim selected As Integer
        For i As Integer = 0 To n - 1 Step 1
            Call PlaceObject(fc_bak, objIDs(i), output(i).X, output(i).Y)
        Next i
        Dim pointsList(freeCells.Length - 1) As Point
        Dim pID As New List(Of Integer)
        Dim np As Integer = -1
        Dim Weight() As Double = Nothing
        Dim desR, R As Double
        If NearWith(n) > -1 Then
            desR = (ActiveObjects(n).Size + ActiveObjects(NearWith(n)).Size + 5) * 0.25 * (1 + Math.Sqrt(2))
            ReDim Weight(UBound(pointsList))
        End If
        For y As Integer = 0 To UBound(freeCells, 2) Step 1
            For x As Integer = 0 To UBound(freeCells, 1) Step 1
                If MayPlaceObject(fc_bak, objIDs(n), x, y) Then
                    np += 1
                    pointsList(np) = New Point(x, y)
                    pID.Add(np)
                    If Not IsNothing(Weight) Then
                        R = Math.Sqrt(SqDist(output(NearWith(n)), pointsList(np)))
                        Weight(np) = Math.Max(comm.Gauss(R, desR, 0.1), 0.0000001)
                    End If
                End If
            Next x
        Next y
        If np = -1 Then
            output(n) = Nothing
            Exit Sub
        End If
        ReDim Preserve pointsList(np), Weight(np)
        If n < UBound(objIDs) Then
            Do While pID.Count > 0
                fc = CType(fc_bak.Clone, Boolean(,))
                selected = comm.RandomSelection(pID, False)
                pID.Remove(selected)
                output(n) = New Point(pointsList(selected).X, pointsList(selected).Y)
                Call PlaceObject(fc, objIDs(n), output(n).X, output(n).Y)
                Call PlaceObjRow(objIDs, n + 1, NearWith, fc, output)
                If Not output(n + 1).IsEmpty Then pID.Clear()
            Loop
            If output(n + 1).IsEmpty Then output(n) = Nothing
        Else
            selected = comm.RandomSelection(pID, False)
            pID.Clear()
            output(n) = New Point(pointsList(selected).X, pointsList(selected).Y)
        End If
    End Sub

    Private Function ObjectBorders(ByRef id As Integer, ByRef x As Integer, ByRef y As Integer) As Location.Borders
        Dim res As Location.Borders
        res.minX = x
        res.minY = y
        res.maxX = x + ActiveObjects(id).Size - 1
        res.maxY = y + ActiveObjects(id).Size - 1
        If ActiveObjects(id).needExpand Then
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
                If m.board(i, j).isBorder Or m.board(i, j).isAttended Then Return False
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
    Private Sub PlaceObject(ByRef m As Map, ByRef id As Integer, ByRef x As Integer, ByRef y As Integer)
        Dim b As Location.Borders = ObjectBorders(id, x, y)
        For j As Integer = b.minY To b.maxY Step 1
            For i As Integer = b.minX To b.maxX Step 1
                m.board(i, j).isAttended = True
            Next i
        Next j
        m.board(x, y).objectID = id
    End Sub
    Private Sub PlaceObject(ByRef m As Map, ByRef id As Integer, ByRef x As Integer, ByRef y As Integer, _
                            ByRef settMap As SettingsMap, ByRef symmId As Integer)
        If symmId < 0 Then
            Call PlaceObject(m, id, x, y)
        Else
            Dim b As Location.Borders = ObjectBorders(id, x, y)
            Dim p(3), plist() As Point
            For k As Integer = 0 To UBound(p) Step 1
                p(k) = New Point(Integer.MaxValue, Integer.MaxValue)
            Next k
            For j As Integer = b.minY To b.maxY Step 1
                For i As Integer = b.minX To b.maxX Step 1
                    plist = symm.ApplySymm(New Point(i, j), settMap.nRaces, m, symmId, 1)
                    For k As Integer = 0 To UBound(plist) Step 1
                        m.board(plist(k).X, plist(k).Y).isAttended = True
                        If p(k).X >= plist(k).X And p(k).Y >= plist(k).Y Then p(k) = New Point(plist(k).X, plist(k).Y)
                    Next k
                Next i
            Next j
            For k As Integer = 0 To UBound(p) Step 1
                If p(k).X < Integer.MaxValue And p(k).Y < Integer.MaxValue Then m.board(p(k).X, p(k).Y).objectID = id
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

End Class

Public Class SymmetryOperations

    ''' <summary>Возвращает точку, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(mapXSize - p.X, mapYSize - p.Y)
    End Function
    ''' <summary>Возвращает точку, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map) As Point
        Return L2(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, повернутую на 180 градусов вокруг оси, находящейся в центре карты</summary>
    Public Function L2(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map) As Location
        Dim r As Location = L.Copy
        Call r.L2Rotation(m, Me)
        Return r
    End Function
    ''' <summary>Возвращает точку, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef p As Point, ByRef mapSize As Integer) As Point
        Return New Point(p.Y, mapSize - p.X)
    End Function
    ''' <summary>Возвращает точку, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map) As Point
        Return L4(p, m.xSize)
    End Function
    ''' <summary>Возвращает локацию, повернутую на 90 градусов вокруг оси, находящейся в центре карты. Только для квадратных карт</summary>
    Public Function L4(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map) As Location
        Dim r As Location = L.Copy
        Call r.L4Rotation(m, Me)
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(p.X, mapYSize - p.Y)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map) As Point
        Return xM(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной оси Ox, и проходящей через центр карты</summary>
    Public Function xM(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map) As Location
        Dim r As Location = L.Copy
        Call r.xReflection(m, Me)
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef p As Point, ByRef mapXSize As Integer, ByRef mapYSize As Integer) As Point
        Return New Point(mapXSize - p.X, p.Y)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map) As Point
        Return yM(p, m.xSize, m.ySize)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной оси Oy, и проходящей через центр карты</summary>
    Public Function yM(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map) As Location
        Dim r As Location = L.Copy
        Call r.yReflection(m, Me)
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef p As Point) As Point
        Return New Point(p.Y, p.X)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map) As Point
        Return xy1M(p)
    End Function
    ''' <summary>Возвращает локацию, отраженную в плоскости, параллельной диагонали {x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy1M(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map) As Location
        Dim r As Location = L.Copy
        Call r.xy1Reflection(m, Me)
        Return r
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef p As Point, ByRef mapSize As Integer) As Point
        Return New Point(mapSize - p.Y, mapSize - p.X)
    End Function
    ''' <summary>Возвращает точку, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map) As Point
        Return xy2M(p, m.xSize)
    End Function
    ''' <summary>Возвращает локация, отраженную в плоскости, параллельной диагонали {-x, y}, и проходящей через центр карты. Только для квадратных карт</summary>
    Public Function xy2M(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map) As Location
        Dim r As Location = L.Copy
        Call r.xy2Reflection(m, Me)
        Return r
    End Function

    ''' <summary>Применяет одну из операций симметрии, разрешенную для двух игроков</summary>
    ''' <param name="ID">id операции</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function TwoPlayersSymm(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map, _
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
    Private Function TwoPlayersSymm(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map, _
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
            For i As Integer = 0 To UBound(L) Step 1
                x += L(i).pos.X
                y += L(i).pos.Y
                a += L(i).gASize
                b += L(i).gBSize
                alpha += L(i).gAlpha
            Next i
            x = CInt(x / L.Length)
            y = CInt(y / L.Length)
            a /= L.Length
            b /= L.Length
            alpha /= L.Length
            Dim res As New Location(New Point(x, y), a, b, alpha, minID)
            Return New Location() {res}
        End If
    End Function

    ''' <summary>Применяет одну из операций симметрии, разрешенную для четырех игроков</summary>
    ''' <param name="ID">id операции</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function FourPlayersSymm(ByRef p As Point, ByRef m As InpenetrableMeshGen.Map, _
                                     ByRef id As Integer, ByRef minSqDist As Integer) As Point()

        Dim res() As Point
        Dim n As Integer = -1
        If id = 0 Then
            ReDim res(3)
            res(0) = New Point(p.X, p.Y)
            For i As Integer = 1 To UBound(res) Step 1
                res(i) = L4(res(i - 1), m)
            Next i
            Return CheckPointsDist(res, minSqDist)
        ElseIf id = 1 Or id = 2 Then
            Dim op1, op2 As Integer
            If id = 1 Then
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
    ''' <param name="ID">id операции</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Private Function FourPlayersSymm(ByRef L As Location, ByRef m As InpenetrableMeshGen.Map, _
                                     ByRef id As Integer, ByRef minSqDist As Integer) As Location()

        Dim res() As Location
        Dim n As Integer = -1
        If id = 0 Then
            ReDim res(3)
            res(0) = L.Copy
            For i As Integer = 1 To UBound(res) Step 1
                res(i) = L4(res(i - 1), m)
            Next i
            Return CheckPointsDist(res, minSqDist)
        ElseIf id = 1 Or id = 2 Then
            Dim op1, op2 As Integer
            If id = 1 Then
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
    Public Function PossibleOperationsList(ByRef nRaces As Integer, ByRef m As InpenetrableMeshGen.Map) As List(Of Integer)
        Return PossibleOperationsList(nRaces, m.xSize, m.ySize)
    End Function

    ''' <summary>Применяет одну из операций симметрии</summary>
    ''' <param name="ID">id операции.
    ''' Для двух рас: 0 - L2, 1 - xM, 2 - yM, 3 - xy1M, 4 - xy2M.
    ''' Для четырех рас: 0 - L4, 1 - xM+yM, 2 - xy1M+xy2M</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Public Function ApplySymm(ByRef p As Point, ByRef nRaces As Integer, ByRef m As InpenetrableMeshGen.Map, _
                              ByRef id As Integer, ByRef minSqDist As Integer) As Point()
        If nRaces = 2 Then
            Return TwoPlayersSymm(p, m, id, minSqDist)
        ElseIf nRaces = 4 Then
            Return FourPlayersSymm(p, m, id, minSqDist)
        Else
            Return Nothing
        End If
    End Function

    ''' <summary>Применяет одну из операций симметрии</summary>
    ''' <param name="ID">id операции.
    ''' Для двух рас: 0 - L2, 1 - xM, 2 - yM, 3 - xy1M, 4 - xy2M.
    ''' Для четырех рас: 0 - L4, 1 - xM+yM, 2 - xy1M+xy2M</param>
    ''' <param name="minSqDist">Квадрат минимального расстояния. Если расстояние меньше, возвращает среднюю точку</param>
    Public Function ApplySymm(ByRef L As Location, ByRef nRaces As Integer, ByRef m As InpenetrableMeshGen.Map, _
                              ByRef id As Integer, ByRef minSqDist As Integer) As Location()
        If nRaces = 2 Then
            Return TwoPlayersSymm(L, m, id, minSqDist)
        ElseIf nRaces = 4 Then
            Return FourPlayersSymm(L, m, id, minSqDist)
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

    Friend Structure Borders
        Dim maxX, minX, maxY, minY As Integer
    End Structure

    Friend Function gAlpha() As Double
        Return alpha
    End Function
    Friend Function gASize() As Double
        Return Asize
    End Function
    Friend Function gBSize() As Double
        Return Bsize
    End Function

    ''' <param name="p">Положение локации</param>
    ''' <param name="a">Половина ширины</param>
    ''' <param name="b">Половина высоты</param>
    ''' <param name="angle">Угол наклона</param>
    ''' <param name="i">Номер локации, больше ноля</param>
    Public Sub New(ByRef p As Point, ByRef a As Double, ByRef b As Double, ByRef angle As Double, ByRef i As Integer)
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
    End Sub

    Public Function Copy() As Location
        Return New Location(pos, Asize, Bsize, alpha, ID)
    End Function

    Private Sub CosSinCalc()
        cos = Math.Cos(alpha)
        sin = Math.Sin(alpha)
    End Sub

    Friend Sub L2Rotation(ByRef m As InpenetrableMeshGen.Map, ByRef symm As SymmetryOperations)
        alpha += Math.PI
        If alpha >= 2 * Math.PI Then alpha -= 2 * Math.PI
        Call CosSinCalc()
        pos = symm.L2(pos, m)
    End Sub
    Friend Sub L4Rotation(ByRef m As InpenetrableMeshGen.Map, ByRef symm As SymmetryOperations)
        alpha += 0.5 * Math.PI
        If alpha >= 2 * Math.PI Then alpha -= 2 * Math.PI
        Call CosSinCalc()
        pos = symm.L4(pos, m)
    End Sub
    Friend Sub xReflection(ByRef m As InpenetrableMeshGen.Map, ByRef symm As SymmetryOperations)
        alpha = 2 * Math.PI - alpha
        Call CosSinCalc()
        pos = symm.xM(pos, m)
    End Sub
    Friend Sub yReflection(ByRef m As InpenetrableMeshGen.Map, ByRef symm As SymmetryOperations)
        alpha = Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.yM(pos, m)
    End Sub
    Friend Sub xy1Reflection(ByRef m As InpenetrableMeshGen.Map, ByRef symm As SymmetryOperations)
        alpha = 0.5 * Math.PI - alpha
        If alpha < 0 Then alpha += 2 * Math.PI
        Call CosSinCalc()
        pos = symm.xy1M(pos, m)
    End Sub
    Friend Sub xy2Reflection(ByRef m As InpenetrableMeshGen.Map, ByRef symm As SymmetryOperations)
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

End Class

Public Class ArrayZoom

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

    Public Function CalcMultiplicator(ByRef size As Integer) As Integer
        Dim maxSize As Integer = 576
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

    Public Function MakeIslandsColorMap(ByRef grid(,) As Integer) As Color(,)

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