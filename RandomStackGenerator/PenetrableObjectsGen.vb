Public Class PenetrableObjectsGen

    Dim symm As New SymmetryOperations
    Dim rndgen As New RndValueGen

    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap)

        If Not settMap.isChecked Then Throw New Exception("Check parameters via settMap.Check()")

        If Not m.complited.ImpenetrableObjectsPlacing_Done Then
            Throw New Exception("Сначала нужно выполнить ImpenetrableObjects.Gen")
        End If

        Dim t0 As Integer = Environment.TickCount

        Dim mustBeFree(,) As Boolean = FunctionMakeMustBeFree(m, settMap)

        Call GenRoads(m, settMap, mustBeFree)
        Call GenForest(m, settMap, mustBeFree)

        Call m.log.Add("Penetrable objects creation: " & Environment.TickCount - t0 & " ms")

        m.complited.PenetrableObjectsPlacing_Done = True

    End Sub

    Private Sub GenRoads(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef mustBeFree(,) As Boolean)

        Dim possible(m.xSize, m.ySize) As Boolean

        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If isPossiblePos(m, x, y, mustBeFree) Then
                    Call SetPossible(m, settMap, possible, x, y, True)
                End If
            Next x
        Next y

        Dim p, current, followng As New List(Of Point)
        Dim nAdded As Integer
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If MayPlaceRoad(m, possible, x, y) Then
                    p = MakeNearestRoadTilesList(m, possible, x, y)
                    If p.Count > 0 Then
                        If rndgen.PRand(0, 1) < settMap.RoadsAmount Then
                            followng.Clear()
                            followng.Add(p.Item(rndgen.RndPos(p.Count, True) - 1))
                            Call SetRoad(m, settMap, x, y, True)
                            Call SetRoad(m, settMap, followng.Item(0).X, followng.Item(0).Y, True)
                            Do While followng.Count > 0
                                current.Clear()
                                For Each item As Point In followng
                                    current.Add(item)
                                Next item
                                followng.Clear()
                                For Each item As Point In current
                                    p = MakeNearestRoadTilesList(m, possible, item.X, item.Y)
                                    nAdded = 0
                                    For Each pos As Point In p
                                        If nAdded < 2 AndAlso rndgen.PRand(0, 1) < settMap.RoadsAmount Then
                                            Call SetRoad(m, settMap, pos.X, pos.Y, True)
                                            followng.Add(pos)
                                            nAdded += 1
                                        Else
                                            Call SetPossible(m, settMap, possible, pos.X, pos.Y, False)
                                        End If
                                    Next pos
                                Next item
                            Loop
                        Else
                            Call SetPossible(m, settMap, possible, x, y, False)
                        End If
                    End If
                End If
            Next x
        Next y
        Dim goAgain As Boolean = True
        Do While goAgain
            goAgain = False
            For y As Integer = 0 To m.ySize Step 1
                For x As Integer = 0 To m.xSize Step 1
                    If m.board(x, y).isRoad AndAlso (IsSquareCorner(m, x, y, 4) OrElse RoadCounter(m, x - 1, x + 1, y - 1, y + 1) = 1) Then
                        Call SetRoad(m, settMap, x, y, False)
                        goAgain = True
                    End If
                Next x
            Next y
        Loop
    End Sub
    Private Function MakeNearestRoadTilesList(ByRef m As Map, ByRef possible(,) As Boolean, ByRef x As Integer, ByRef y As Integer) As List(Of Point)
        Dim p As New List(Of Point)
        For k As Integer = 0 To 1 Step 1
            For d As Integer = -1 To 1 Step 2
                Dim dx As Integer = x + d * k
                Dim dy As Integer = y + d * (1 - k)
                If MayPlaceRoad(m, possible, dx, dy) Then p.Add(New Point(dx, dy))
            Next d
        Next k
        If p.Count < 2 Then Return p
        Dim res As New List(Of Point)
        Dim n As Integer = p.Count - 1
        For i As Integer = 1 To n Step 1
            Dim r As Integer = rndgen.RndPos(p.Count, True) - 1
            res.Add(p.Item(r))
            p.RemoveAt(r)
        Next i
        res.Add(p.Item(0))
        Return res
    End Function
    Private Sub SetPossible(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef possible(,) As Boolean, ByRef x As Integer, ByRef y As Integer, ByRef whatset As Boolean)
        If m.symmID > -1 Then
            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
            For i As Integer = 0 To UBound(p) Step 1
                possible(p(i).X, p(i).Y) = whatset
            Next i
        Else
            possible(x, y) = whatset
        End If
    End Sub
    Private Sub SetRoad(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef x As Integer, ByRef y As Integer, ByRef whatset As Boolean)
        If m.symmID > -1 Then
            Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
            For i As Integer = 0 To UBound(p) Step 1
                m.board(p(i).X, p(i).Y).isRoad = whatset
            Next i
        Else
            m.board(x, y).isRoad = whatset
        End If
    End Sub
    Private Function MayPlaceRoad(ByRef m As Map, ByRef possible(,) As Boolean, ByRef x As Integer, ByRef y As Integer) As Boolean
        If x < 0 Or y < 0 Or x > m.xSize Or y > m.ySize Then Return False
        If m.board(x, y).isRoad Then Return False
        If Not possible(x, y) Then Return False
        If IsSquareCorner(m, x, y, 3) Then Return False
        Return True
    End Function
    Private Function IsSquareCorner(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, ByRef threshold As Integer) As Boolean
        For y1 As Integer = y - 1 To y Step 1
            If y1 >= 0 And y1 < m.ySize Then
                For x1 As Integer = x - 1 To x Step 1
                    If x1 >= 0 And x1 < m.xSize Then
                        If RoadCounter(m, x1, x1 + 1, y1, y1 + 1) >= threshold Then Return True
                    End If
                Next x1
            End If
        Next y1
        Return False
    End Function
    Private Function RoadCounter(ByRef m As Map, ByRef x1 As Integer, ByRef x2 As Integer, ByRef y1 As Integer, ByRef y2 As Integer) As Integer
        Dim n As Integer
        For y As Integer = y1 To y2 Step 1
            If y >= 0 And y <= m.ySize Then
                For x As Integer = x1 To x2 Step 1
                    If x >= 0 And x <= m.xSize Then
                        If m.board(x, y).isRoad Then n += 1
                    End If
                Next x
            End If
        Next y
        Return n
    End Function

    Private Sub GenForest(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef mustBeFree(,) As Boolean)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If isPossiblePos(m, x, y, mustBeFree) AndAlso rndgen.PRand(0, 1) < settMap.ForestAmount Then
                    If m.symmID > -1 Then
                        Dim p() As Point = symm.ApplySymm(New Point(x, y), settMap.nRaces, m, 1)
                        For i As Integer = 0 To UBound(p) Step 1
                            m.board(p(i).X, p(i).Y).isForest = True
                        Next i
                    Else
                        m.board(x, y).isForest = True
                    End If
                End If
            Next x
        Next y
    End Sub

    Private Function FunctionMakeMustBeFree(ByRef m As Map, ByRef settMap As Map.SettingsMap) As Boolean(,)
        Dim mustBeFree(m.xSize, m.ySize) As Boolean
        Dim pos As New List(Of Point)
        Dim skip As Boolean
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If Not m.Loc(m.board(x, y).locID(0) - 1).IsObtainedBySymmery And m.board(x, y).objectID = DefMapObjects.Types.Mine Then
                    skip = False
                    pos.Clear()
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 1)
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If mustBeFree(i, j) Then
                                skip = True
                                i = b.maxX
                                j = b.maxY
                            Else
                                If isPossiblePos(m, i, j, mustBeFree) Then pos.Add(New Point(i, j))
                            End If
                        Next i
                    Next j
                    If Not skip Then
                        If pos.Count > 0 Then
                            Dim selected As Point = pos.Item(rndgen.RndPos(pos.Count, True) - 1)
                            If m.symmID > -1 Then
                                Dim p() As Point = symm.ApplySymm(selected, settMap.nRaces, m, 1)
                                For i As Integer = 0 To UBound(p) Step 1
                                    mustBeFree(p(i).X, p(i).Y) = True
                                Next i
                            Else
                                mustBeFree(selected.X, selected.Y) = True
                            End If
                        Else
                            Throw New Exception("Возле шахты в точке (" & x & "; " & y & ") невозможно поставить жезл")
                        End If
                    End If
                End If
            Next x
        Next y
        Return mustBeFree
    End Function
    Private Function isPossiblePos(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, ByRef mustBeFree(,) As Boolean) As Boolean
        Dim c As Map.Cell = m.board(x, y)

        If m.Loc(c.locID(0) - 1).IsObtainedBySymmery Then Return False

        If c.isAttended Then Return False
        If c.isBorder Then Return False
        If c.isForest Then Return False
        If c.isRoad Then Return False
        If c.isWater Then Return False

        If mustBeFree(x, y) Then Return False

        Return True
    End Function

End Class
