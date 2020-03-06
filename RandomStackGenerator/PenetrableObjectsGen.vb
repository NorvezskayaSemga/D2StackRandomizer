Public Class PenetrableObjectsGen

    Dim symm As New SymmetryOperations
    Dim rndgen As New RndValueGen

    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap)

        If Not m.complited.ImpenetrableObjectsPlacing_Done Then
            Throw New Exception("Сначала нужно выполнить RaceGen.Gen")
        End If

        Dim t0 As Integer = Environment.TickCount

        Dim mustBeFree(,) As Boolean = FunctionMakeMustBeFree(m, settMap)

        Call GenRoads(m, settMap, mustBeFree)
        Call GenForest(m, settMap, mustBeFree)

        Dim t1 As Integer = Environment.TickCount
        Console.WriteLine("Penetrable objects placing: " & t1 - t0)

        m.complited.PenetrableObjectsPlacing_Done = True

    End Sub

    Private Sub GenRoads(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef mustBeFree(,) As Boolean)

    End Sub

    Private Sub GenForest(ByRef m As Map, ByRef settMap As Map.SettingsMap, ByRef mustBeFree(,) As Boolean)
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If isPossiblePos(m, x, y, mustBeFree) AndAlso rndgen.PRand(0, 1) <= settMap.ForestAmount Then
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
        For y As Integer = 0 To m.ySize Step 1
            For x As Integer = 0 To m.xSize Step 1
                If Not m.Loc(m.board(x, y).locID.Item(0) - 1).IsObtainedBySymmery And m.board(x, y).objectID = 8 Then
                    pos.Clear()
                    Dim b As Location.Borders = ImpenetrableMeshGen.NearestXY(x, y, m.xSize, m.ySize, 1)
                    For j As Integer = b.minY To b.maxY Step 1
                        For i As Integer = b.minX To b.maxX Step 1
                            If isPossiblePos(m, i, j, mustBeFree) Then pos.Add(New Point(i, j))
                        Next i
                    Next j
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
            Next x
        Next y
        Return mustBeFree
    End Function
    Private Function isPossiblePos(ByRef m As Map, ByRef x As Integer, ByRef y As Integer, ByRef mustBeFree(,) As Boolean) As Boolean
        Dim c As Map.Cell = m.board(x, y)

        If m.Loc(c.locID.Item(0) - 1).IsObtainedBySymmery Then Return False

        If c.isAttended Then Return False
        If c.isBorder Then Return False
        If c.isForest Then Return False
        If c.isRoad Then Return False
        If c.isWater Then Return False

        If mustBeFree(x, y) Then Return False

        Return True
    End Function

End Class
