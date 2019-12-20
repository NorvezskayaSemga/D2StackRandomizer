Imports System.Drawing
Imports System.ComponentModel
Imports System.Threading.Tasks

Public Class InpenetrableMeshGen

    Public Structure Cell
        Dim locID As List(Of Integer)
        Dim isBorder As Boolean
    End Structure

    Public Structure Map
        Dim Loc As Location()
        Dim board(,) As Cell
        Dim xSize As Integer
        Dim ySize As Integer
    End Structure

    Private Structure PrepareToRaceLocGenResult
        Dim DminimumDist As Double
        Dim IminimumDist As Integer
        Dim possiblePoints() As Point
        Dim ppIDs As List(Of Integer)
        Dim raceLocs() As Location
        Dim equalDist(,) As List(Of Integer)
    End Structure

    Dim rndgen As New RndValueGen
    Dim comm As New Common

    Public Function Gen(ByRef xSize As Integer, ByRef ySize As Integer, nRaces As Integer, _
                        ByRef RaceLocRadius As Integer, ByRef CommonLocRadius As Integer, _
                        ByRef maxEccentricityDispersion As Double, _
                        ByRef maxRadiusDispersion As Double) As Map

        Dim m As Map = UnsymmPlaceRaceLocations(xSize, ySize, nRaces, RaceLocRadius)
        Call UnsymmPlaceCommonLocs(m, maxEccentricityDispersion, maxRadiusDispersion, CommonLocRadius)
        Call UnsymmSetLocIdToCells(m)
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

    Private Function UnsymmPlaceRaceLocations(ByVal xSize As Integer, ByVal ySize As Integer, _
                                              ByRef nRaces As Integer, ByVal RaceLocRadius As Integer) As Map

        Dim res As New Map
        Dim ok As Boolean = False
        Dim tryagain As Boolean = False
        Dim borderPoints() As Point = Nothing
        Dim id, t As Integer

        Dim prepResult As PrepareToRaceLocGenResult = PrepareToRaceLocGen(xSize, ySize, nRaces, RaceLocRadius)
        Dim raceLocs() As Location = prepResult.raceLocs
        Dim SelID, tmpID As New List(Of Integer)
        Do While Not ok
            id = comm.RandomSelection(prepResult.ppIDs, True)
            raceLocs(0).pos = New Point(prepResult.possiblePoints(id).X, prepResult.possiblePoints(id).Y)

            SelID.Clear()
            For Each i As Integer In prepResult.equalDist(raceLocs(0).pos.X, raceLocs(0).pos.Y)
                SelID.Add(i)
            Next i
            For n As Integer = 1 To nRaces - 1 Step 1
                t = comm.RandomSelection(SelID, True)
                raceLocs(n).pos = New Point(prepResult.possiblePoints(t).X, prepResult.possiblePoints(t).Y)

                If n < nRaces - 1 Then
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
                ok = TestRaceLocations(raceLocs, nRaces, prepResult)
            End If
        Loop
        res.Loc = raceLocs
        res.xSize = xSize
        res.ySize = ySize
        Call ResetBoard(res)
        For i As Integer = 0 To UBound(res.Loc) Step 1
            Call PlaceLoc(res, res.Loc(i))
        Next i
        Return res
    End Function

    Private Function PrepareToRaceLocGen(ByRef xSize As Integer, ByRef ySize As Integer, ByRef nRaces As Integer, _
                                         ByRef RaceLocRadius As Integer) As PrepareToRaceLocGenResult
        Dim result As New PrepareToRaceLocGenResult
        ReDim result.raceLocs(nRaces - 1)
        For i As Integer = 0 To nRaces - 1 Step 1
            result.raceLocs(i) = GenLocSize(0.15, RaceLocRadius, i + 1)
        Next i

        If Math.Max(xSize, ySize) < 2 * RaceLocRadius + 10 Then Throw New Exception("Слишком большой радиус начальных локаций")
        If RaceLocRadius < 7 Then Throw New Exception("Слишком маленький радиус начальных локаций")

        Dim r, m As Double
        Dim n As Integer
        If nRaces = 4 Then
            r = 0.45 * (Math.Sqrt(((xSize - 2 * RaceLocRadius) ^ 2) _
                                + ((ySize - 2 * RaceLocRadius) ^ 2) - 2))
            m = CInt(0.5 * Math.Min(r, Math.Min(xSize, ySize) - 2 * RaceLocRadius))
        ElseIf nRaces = 3 Then
            r = 0.5 * Math.Sqrt(3) * (Math.Min(xSize, ySize) - 2 * RaceLocRadius)
            m = CInt(0.25 * Math.Sqrt(3) * r)
        ElseIf nRaces = 2 Then
            r = 0.5 * (xSize + ySize - 4 * RaceLocRadius)
            m = CInt(r / Math.Sqrt(2))
        Else
            Throw New Exception("Неожиданное аколичество рас: " & nRaces)
            r = 2 * RaceLocRadius + 2
            m = 0
        End If
        result.DminimumDist = r
        result.IminimumDist = CInt((r - 2) ^ 2)

        Dim dX As Integer = CInt(Math.Min(0.5 * xSize - RaceLocRadius - 1, m))
        Dim dY As Integer = CInt(Math.Min(0.5 * ySize - RaceLocRadius - 1, m))
        Dim x1 As Integer = CInt(0.5 * xSize - dX)
        Dim x2 As Integer = CInt(0.5 * xSize + dX)
        Dim y1 As Integer = CInt(0.5 * ySize - dY)
        Dim y2 As Integer = CInt(0.5 * ySize + dY)

        result.ppIDs = New List(Of Integer)
        ReDim result.possiblePoints((xSize + 1) * (ySize + 1) - 1)
        n = -1
        For x As Integer = 0 To xSize Step 1
            If x >= RaceLocRadius And x <= xSize - RaceLocRadius Then
                For y As Integer = 0 To ySize Step 1
                    If y >= RaceLocRadius And y <= ySize - RaceLocRadius Then
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

        Dim borderPoints(2 * (xSize + ySize) - 1) As Point
        n = -1
        For i As Integer = 0 To xSize Step 1
            n += 1
            borderPoints(n) = New Point(i, 0)
            n += 1
            borderPoints(n) = New Point(i, ySize)
        Next i
        For i As Integer = 1 To ySize - 1 Step 1
            n += 1
            borderPoints(n) = New Point(0, i)
            n += 1
            borderPoints(n) = New Point(xSize, i)
        Next i

        Dim distToBorder(xSize, ySize) As Integer
        ReDim result.equalDist(xSize, ySize)
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
                                       ByRef prep As PrepareToRaceLocGenResult) As Boolean
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
            If minD1 * 1.5 < minD2 Then Return False
        Next i
        Return True
    End Function

    Private Sub UnsymmPlaceCommonLocs(ByRef m As Map, ByRef maxDispersion As Double, ByRef maxRadiusDispersion As Double, _
                                      ByRef AverageRadius As Integer)
        Dim id As Integer = m.Loc.Length + 1
        Dim dynRDisp As Double = maxRadiusDispersion
        Dim dynRadius As Double = AverageRadius
        Dim dynBaseR As Double = AverageRadius
        Dim b As Location.Borders
        Dim R As Integer
        Dim locPlist As New List(Of Point)
        Dim possiblePoints((m.xSize + 1) * (m.ySize + 1) - 1) As Point
        Dim IDs As New List(Of Integer)
        Dim add As Boolean
        Do While True
            R = CInt(rndgen.PRand(1 - dynRDisp, 1 + dynRDisp) * dynRadius)
            Dim loc As Location = GenLocSize(maxDispersion, R, id)
            b = loc.XYborders(Integer.MaxValue, Integer.MaxValue, Integer.MinValue, Integer.MinValue)

            locPlist.Clear()
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
            If IDs.Count > 0 Then
                Dim pid As Integer = comm.RandomSelection(IDs, True)
                loc.pos = New Point(possiblePoints(pid).X, possiblePoints(pid).Y)
                ReDim Preserve m.Loc(m.Loc.Length)
                m.Loc(UBound(m.Loc)) = loc
                Call PlaceLoc(m, m.Loc(UBound(m.Loc)))
                id += 1
            Else
                Dim newDisp As Double = 0.9 * dynRDisp
                If newDisp < 0.001 Then
                    dynRDisp = maxRadiusDispersion
                    dynBaseR *= 0.9
                    dynRadius = dynBaseR
                    If dynBaseR < 0.5 * AverageRadius Then Exit Do
                Else
                    dynRadius *= (1 - dynRDisp) / (1 - newDisp)
                    dynRDisp = newDisp
                End If
            End If
        Loop
    End Sub

    Private Sub UnsymmSetLocIdToCells(ByRef m As Map)
        Call ResetBoard(m)
        For Each Loc As Location In m.Loc
            m.board(Loc.pos.X, Loc.pos.Y).locID.Add(Loc.ID)
        Next Loc
        Dim allPoints((m.xSize + 1) * (m.ySize - 1)) As Point
        Dim pID(UBound(allPoints)) As Integer
        Dim Weight(UBound(allPoints)) As Double
        Dim IDs As New List(Of Integer)
        Dim b As Location.Borders
        Dim n As Integer = 0
        Dim minweight As Double = 10 ^ -9

        Do While n > -1
            IDs.Clear()
            n = -1
            For x As Integer = 0 To m.xSize Step 1
                For y As Integer = 0 To m.ySize Step 1
                    If m.board(x, y).locID.Count = 0 Then
                        b = NearestXY(x, y, m.xSize, m.ySize, 1)
                        For i As Integer = b.minX To b.maxX Step 1
                            For j As Integer = b.minY To b.maxY Step 1
                                If m.board(i, j).locID.Count > 0 Then
                                    n += 1
                                    If n > UBound(allPoints) Then
                                        ReDim Preserve allPoints(2 * allPoints.Length - 1)
                                        ReDim Preserve pID(UBound(allPoints)), Weight(UBound(allPoints))
                                    End If
                                    allPoints(n) = New Point(x, y)
                                    pID(n) = m.board(i, j).locID.Item(0)
                                    Weight(n) = m.Loc(pID(n) - 1).pWeight(allPoints(n))
                                    Weight(n) = Math.Max(Weight(n), minweight)
                                    IDs.Add(n)
                                End If
                            Next j
                        Next i
                    End If
                Next y
            Next x
            If n > -1 Then
                Dim s As Integer = comm.RandomSelection(IDs, Weight, True)
                m.board(allPoints(s).X, allPoints(s).Y).locID.Add(pID(s))
            End If
        Loop
    End Sub

    Private Sub ResetBoard(ByRef m As Map)
        ReDim m.board(m.xSize, m.ySize)
        For x As Integer = 0 To m.xSize Step 1
            For y As Integer = 0 To m.ySize Step 1
                m.board(x, y).locID = New List(Of Integer)
            Next y
        Next x
    End Sub
    Private Function GenLocSize(ByRef maxDispersion As Double, ByRef Radius As Integer, ByRef id As Integer) As Location
        Dim r, a As Double
        r = rndgen.PRand(1 - maxDispersion, 1 + maxDispersion)
        a = rndgen.PRand(0, Math.PI)
        Return New Location(New Point(0, 0), Radius * r, Radius / r, a, id)
    End Function
    Private Function SqDist(ByRef p1 As Point, ByRef p2 As Point) As Integer
        Dim dx As Integer = p1.X - p2.X
        Dim dy As Integer = p1.Y - p2.Y
        Return dx * dx + dy * dy
    End Function
End Class

Public Class Location

    Public ID As Integer
    Public pos As Point
    Private invSqA, invSqB, cos, sin As Double
    Private Asize, Bsize As Double

    Friend Structure Borders
        Dim maxX, minX, maxY, minY As Integer
    End Structure

    ''' <param name="p">Положение локации</param>
    ''' <param name="a">Половина ширины</param>
    ''' <param name="b">Половина высоты</param>
    ''' <param name="angle">Угол наклона</param>
    ''' <param name="i">Номер локации, Больше 0</param>
    Friend Sub New(ByRef p As Point, ByRef a As Double, ByRef b As Double, ByRef angle As Double, ByRef i As Integer)
        pos = New Point(p.X, p.Y)
        cos = Math.Cos(angle)
        sin = Math.Sin(angle)
        invSqA = 1 / (a * a)
        invSqB = 1 / (b * b)
        Asize = a
        Bsize = b
        ID = i
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
        Dim w1 As Double = Gauss(d1, 0.5 * Asize)
        Dim w2 As Double = Gauss(d2, 0.5 * Bsize)
        Return w1 * w2
    End Function
    Friend Function pWeight(ByRef P As Point) As Double
        Return pWeight(P.X, P.Y)
    End Function
    Private Function Gauss(ByRef X As Double, ByRef sigma As Double) As Double
        Dim s As Double = 1 / sigma
        Return Math.Exp(-0.5 * (X * sigma) ^ 2) * sigma
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
             For J As Integer = 0 To imgXSize Step 1
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