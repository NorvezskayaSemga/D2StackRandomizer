Imports System.Drawing
Imports System.ComponentModel
Imports System.Threading.Tasks

Public Class InpenetrableMeshGen

    Private Structure Cell
        Dim pos As Point
        Dim locID As List(Of Integer)
        Dim isBorder As Boolean
    End Structure

    Private Structure Map
        Dim Loc As Location()
        Dim board(,) As Cell
    End Structure

    Public RaceLocRadius As Integer = 8
    Dim rndgen As New RndValueGen

    Public Function Gen(ByRef xSize As Integer, ByRef ySize As Integer, nRaces As Integer) As Integer(,)


        Dim m As Map = UnsymmPlaceRaceLocations(xSize, ySize, nRaces)
        Dim res(xSize, ySize) As Integer
        For i As Integer = 0 To UBound(m.Loc) Step 1
            res(m.Loc(i).pos.X, m.Loc(i).pos.Y) = i + 1
        Next i
        Return res
    End Function

    Private Function UnsymmPlaceRaceLocations(ByVal xSize As Integer, ByVal ySize As Integer, nRaces As Integer) As Map
        Dim minimumDist, minDistCenter() As Integer
        Call CalcMinDist(xSize, ySize, nRaces, minimumDist, minDistCenter)
        Dim res As New Map
        ReDim res.board(xSize, ySize)
        Dim raceLocs() As Location = GenRaceLocsSizes(nRaces)
        Dim ok As Boolean = False
        Dim borderPoints() As Point = Nothing
        Dim RPos() As Point = Nothing
        Call prepareTemporalPointsArrays(xSize, ySize, nRaces, borderPoints, RPos)

        Do While Not ok
            Parallel.For(0, nRaces * 2, _
             Sub(i As Integer)
                 If (i Mod 2) = 0 Then
                     RPos(CInt(i / 2)).X = RaceLocRadius + rndgen.RndPos(xSize + 1 - 2 * RaceLocRadius, False) - 1
                 Else
                     RPos(CInt((i - 1) / 2)).Y = RaceLocRadius + rndgen.RndPos(ySize + 1 - 2 * RaceLocRadius, False) - 1
                 End If
             End Sub)
            ok = TestRaceLocations(RPos, borderPoints, nRaces, minimumDist)
        Loop
        For i As Integer = 0 To nRaces - 1 Step 1
            raceLocs(i).pos = RPos(i)
        Next i
        res.Loc = raceLocs
        Return res
    End Function
    Private Sub prepareTemporalPointsArrays(ByRef xSize As Integer, ByRef ySize As Integer, ByRef nRaces As Integer, _
                                            ByRef borderPoints_out() As Point, ByRef GPos_out() As Point)
        ReDim borderPoints_out(2 * (xSize + ySize) - 1)
        Dim n As Integer = -1
        For i As Integer = 0 To xSize Step 1
            n += 1
            borderPoints_out(n) = New Point(i, 0)
            n += 1
            borderPoints_out(n) = New Point(i, ySize)
        Next i
        For i As Integer = 1 To ySize - 1 Step 1
            n += 1
            borderPoints_out(n) = New Point(0, i)
            n += 1
            borderPoints_out(n) = New Point(xSize, i)
        Next i
        ReDim GPos_out(nRaces - 1)
        For i As Integer = 0 To nRaces - 1 Step 1
            GPos_out(i) = New Point
        Next i
    End Sub
    Private Function GenRaceLocPoint(ByRef xSize As Integer, ByRef ySize As Integer, _
                                     ByRef minDistCenter() As Integer) As Point
        Dim x As Double = rndgen.Rand(CDbl(RaceLocRadius), xSize - RaceLocRadius)
        Dim y As Double = rndgen.Rand(CDbl(RaceLocRadius), ySize - RaceLocRadius)
        'если точка слишком близко, строим луч от центра через эту точку
        'смотрим длину запрещенного и разрешенного отрезка
        'переносим точку с запрещенного отрезка на разрешенный в зависимости от положения точки на запрещенном


    End Function
    Private Function GenRaceLocsSizes(ByRef nRaces As Integer) As Location()
        Dim raceLocs(nRaces - 1) As Location
        For i As Integer = 0 To nRaces - 1 Step 1
            raceLocs(i) = GenLocSize(0.15)
        Next i
        Return raceLocs
    End Function
    Private Function GenLocSize(ByRef maxDispersion As Double) As Location
        Dim r, a As Double
        r = rndgen.PRand(1 - maxDispersion, 1 + maxDispersion)
        a = rndgen.PRand(0, Math.PI)
        Return New Location(New Point(0, 0), RaceLocRadius * r, RaceLocRadius / r, a)
    End Function
    Private Sub CalcMinDist(ByRef xSize As Integer, ByRef ySize As Integer, nRaces As Integer, _
                            ByRef minimumDist_out As Integer, ByRef minDistCenter_out() As Integer)
        Dim r, m As Double
        If nRaces = 4 Then
            r = 0.5 * Math.Sqrt(((xSize - 2 * RaceLocRadius) ^ 2) _
                              + ((ySize - 2 * RaceLocRadius) ^ 2))
            m = CInt(0.5 * r)
        ElseIf nRaces = 3 Then
            r = 0.5 * Math.Sqrt(3) * Math.Min(xSize, ySize)
            m = CInt(0.25 * Math.Sqrt(3) * r)
        ElseIf nRaces = 2 Then
            r = 0.5 * (xSize + ySize - 4 * RaceLocRadius)
            m = CInt(r * Math.Sqrt(2))
        Else
            Throw New Exception("Неожиданное аколичество рас: " & nRaces)
            minimumDist_out = 2 * RaceLocRadius
            minDistCenter_out = New Integer() {0, 0}
            Exit Sub
        End If
        minDistCenter_out = New Integer() {CInt(Math.Min(0.5 * xSize - RaceLocRadius - 1, m)), _
                                           CInt(Math.Min(0.5 * ySize - RaceLocRadius - 1, m))}
        minimumDist_out = CInt(r)
        minimumDist_out = (minimumDist_out - 2) * (minimumDist_out - 2)
    End Sub
    Private Function TestRaceLocations(ByRef RPos() As Point, ByRef borderPoints() As Point, _
                                       ByRef nRaces As Integer, ByRef minimumDist As Integer) As Boolean
        Dim D, maxD, minD1, minD2 As Integer
        For i As Integer = 0 To nRaces - 1 Step 1
            minD1 = -1
            For j As Integer = 0 To nRaces - 1 Step 1
                If Not j = i Then
                    D = SqDist(RPos(i), RPos(j))
                    If D < minimumDist Then Return False
                    If minD1 > -1 Then
                        If minD1 > D Then
                            minD2 = minD1
                            minD1 = D
                        ElseIf minD2 < D Then
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
        Dim u As Integer = UBound(borderPoints)
        For i As Integer = 0 To nRaces - 1 Step 1
            D = 0
            For j As Integer = 0 To u Step 1
                D += SqDist(RPos(i), borderPoints(j))
            Next j
            If i > 0 Then
                If minD1 > D Then
                    minD1 = D
                ElseIf maxD < D Then
                    maxD = D
                End If
            Else
                minD1 = D
                maxD = D
            End If
            If maxD > 1.5 * minD1 Then Return False
        Next i
        Return True
    End Function

    Private Function SqDist(ByRef p1 As Point, ByRef p2 As Point) As Integer
        Dim dx As Integer = p1.X - p2.X
        Dim dy As Integer = p1.Y - p2.Y
        Return dx * dx + dy * dy
    End Function
End Class

Class Location

    Public pos As Point
    Dim invSqA, invSqB, cos, sin As Double

    ''' <param name="p">Положение локации</param>
    ''' <param name="a">Половина ширины</param>
    ''' <param name="b">Половина высоты</param>
    ''' <param name="angle">Угол наклона</param>
    Friend Sub New(ByRef p As Point, ByRef a As Double, ByRef b As Double, ByRef angle As Double)
        pos = New Point(p.X, p.Y)
        cos = Math.Cos(angle)
        sin = Math.Sin(angle)
        invSqA = 1 / (a * a)
        invSqB = 1 / (b * b)
    End Sub

    Friend Function IsInside(ByRef Loc As Location, ByRef P As Point) As Boolean
        Dim x1 As Double = P.X * cos
        Dim x2 As Double = P.Y * sin
        Dim y1 As Double = -P.X * sin
        Dim y2 As Double = P.Y * cos
        x1 += x2
        y1 += y2
        If x1 * x1 * invSqA + y1 * y1 * invSqB > 1 Then
            Return False
        Else
            Return True
        End If
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