Imports RandomStackGenerator

Friend Class StartForm

    Dim genmesh As New ImpenetrableMeshGen
    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector
    Dim symm As New SymmetryOperations
    Dim stackstats As New StackPowerGen
    Dim racegen As New RaceGen
    Dim names As New SetName
    Dim watergenerator As New WaterGen
    Dim comm As New Common
    Dim objplace As ImpenetrableObjects
    Dim penOnjGen As New PenetrableObjectsGen
    Dim ObjectsSize As New Dictionary(Of String, Size)

    Private Sub GenButton_Click() Handles GenButton.Click

        Call comm.ReadExcludedObjectsList({"%default%"})
        Call ReadObjSize()

        objplace = New ImpenetrableObjects(ObjectsSize, {"%default%"}, {"%default%"}, {"%default%"}, ReadSpells)

        Dim grid As Map
        Dim races As Integer
        If sel1.Checked Then
            races = 2
        ElseIf sel2.Checked Then
            races = 3
        Else
            races = 4
        End If

        Dim sM As Map.SettingsMap
        sM.xSize = 95
        sM.ySize = 95
        sM.RaceLocsDistTolerance = 0.2
        sM.nRaces = races
        sM.minPassDist = 7
        sM.minPassWidth = 1.1
        sM.AddGuardsBetweenLocations = True
        sM.LocExpRatio = 2
        sM.PassGuardsPowerMultiplicator = 2
        sM.Wealth = 0.8
        sM.WaterAmount = 0
        sM.SpellsMaxLevel = 5
        sM.RoadsAmount = 0.3
        sM.ForestAmount = 0.5

        Dim sR As Map.SettingsLoc
        sR.AverageRadius = 20
        sR.maxEccentricityDispersion = 0.15
        sR.maxRadiusDispersion = 0
        sR.maxGoldMines = 1
        sR.maxManaSources = 2
        sR.maxCities = 0
        sR.maxMages = 1
        sR.maxMercenaries = 1
        sR.maxRuins = 0
        sR.maxTrainers = 0
        sR.maxVendors = 1
        sR.minStackToStackDist = 4
        sR.expAmount = 6000

        sR.mageGlobalSpellsEnabled = False
        sR.mageSpellsCount = 5
        sR.mageSpellsMaxLevel = 2
        sR.mageSpellsMinLevel = 1
        sR.merchItemsCost = 2000
        sR.merchMaxItemCost = 1000
        sR.merchMinItemCost = 0
        sR.mercenariesCount = 3
        sR.mercenariesMaxExpBar = 900
        sR.mercenariesMinExpBar = 200

        Dim sC As Map.SettingsLoc
        sC.AverageRadius = 15
        sC.maxEccentricityDispersion = 0.4
        sC.maxRadiusDispersion = 0.3

        sC.maxGoldMines = 0
        sC.maxManaSources = 1
        sC.maxCities = 0
        sC.maxMages = 1
        sC.maxMercenaries = 1
        sC.maxRuins = 0
        sC.maxTrainers = 0
        sC.maxVendors = 1

        sC.minStackToStackDist = 5
        sC.expAmount = 15000

        sC.mageGlobalSpellsEnabled = True
        sC.mageSpellsCount = 4
        sC.mageSpellsMaxLevel = 4
        sC.mageSpellsMinLevel = 2
        sC.merchItemsCost = 4000
        sC.merchMaxItemCost = 1500
        sC.merchMinItemCost = 100
        sC.mercenariesCount = 5
        sC.mercenariesMaxExpBar = 1700
        sC.mercenariesMinExpBar = 1000

        Dim gt As Integer = 10000
again:
        If Not SymmCheckBox.Checked Then
            grid = genmesh.UnsymmGen(sM, sR, sC, gt)
        Else
            grid = genmesh.SymmGen(sM, sR, sC, gt)
        End If

        If Not grid.TestMap = "" Then
            Console.WriteLine(grid.TestMap)
            Exit Sub
        End If

        If Not IsNothing(grid) Then
            Dim staclocgen As New StackLocationsGen
            If Not staclocgen.Gen(grid, sM, sR, sC, gt) Then GoTo again
        Else
            Exit Sub
        End If

        If Not grid.TestMap = "" Then
            Console.WriteLine(grid.TestMap)
            Exit Sub
        End If

        Call stackstats.Gen(grid, sM, sR, sC)
        Call watergenerator.Gen(grid, sM)
        Call racegen.Gen(grid, Nothing)

        Call objplace.Gen(grid, sM, sR, sC)

        Call penOnjGen.Gen(grid, sM)

        Call ShowResult(grid)

    End Sub
    Public Sub ShowResult(ByRef grid As Map)
        Dim t(grid.xSize, grid.ySize) As Integer
        For x As Integer = 0 To grid.xSize Step 1
            For y As Integer = 0 To grid.ySize Step 1
                If grid.board(x, y).locID.Count = 0 Or Not grid.board(x, y).isBorder Then
                    t(x, y) = 0
                End If
                If grid.board(x, y).isAttended Then
                    t(x, y) = 51 + 2 * grid.board(x, y).objectID
                ElseIf grid.board(x, y).isPass Then
                    't(x, y) = 90
                End If
                If grid.board(x, y).Penetrable Then
                    't(x, y) = 100
                End If
                If grid.board(x, y).GuardLoc Then
                    t(x, y) = 125
                End If
                If grid.board(x, y).PassGuardLoc Then
                    t(x, y) = 185
                End If
                If grid.board(x, y).isBorder Then
                    t(x, y) = grid.board(x, y).objRace.Item(0)
                ElseIf grid.board(x, y).isWater Then
                    't(x, y) += 200
                End If
                If grid.board(x, y).isForest Then
                    t(x, y) = 135
                End If
                If grid.board(x, y).isRoad Then
                    t(x, y) = 145
                End If
            Next y
        Next x
        For x As Integer = 0 To grid.xSize Step 1
            For y As Integer = 0 To grid.ySize Step 1
                If grid.board(x, y).isBorder Then
                    If Not IsNothing(grid.board(x, y).objectName) AndAlso grid.board(x, y).objectName.Length > 0 Then
                        Dim s As Size = ObjectsSize.Item(grid.board(x, y).objectName)
                        For j As Integer = y To y + s.Height - 1 Step 1
                            For i As Integer = x To x + s.Width - 1 Step 1
                                If Not grid.board(i, j).isBorder Or t(i, j) > 500 Then
                                    Throw New Exception
                                End If
                                't(i, j) = 501
                            Next i
                        Next j
                    End If
                End If
            Next y
        Next x
        Dim mult As Integer = zoom.CalcMultiplicator(Math.Max(UBound(t, 1), UBound(t, 2)) + 1, 576)
        Dim mgrid(,) As Integer = zoom.Zoom(t, mult)
        Dim c(,) As Color = draw.MakeColorMap(mgrid)
        Dim xsize As Integer = UBound(c, 1)
        Dim ysize As Integer = UBound(c, 2)
        Dim img As Bitmap
        If IsNothing(Me.PictureBox1.Image) OrElse PictureBox1.Image.Width <> xsize + 1 OrElse PictureBox1.Image.Height <> ysize + 1 Then
            img = New Bitmap(xsize + 1, ysize + 1)
        Else
            img = PictureBox1.Image
        End If
        For i As Integer = 0 To xsize Step 1
            For j As Integer = 0 To ysize Step 1
                img.SetPixel(i, j, c(i, j))
            Next j
        Next i
        PictureBox1.Image = img
    End Sub

    Private Function ReadSpells() As Dictionary(Of String, AllDataStructues.Spell)
        Dim spells() As String = comm.TxtSplit(My.Resources.TestSpells)
        Dim rspells() As String = comm.TxtSplit(My.Resources.TestSpellsRace)
        Dim res As New Dictionary(Of String, AllDataStructues.Spell)
        For i As Integer = 1 To UBound(spells) Step 1
            Dim s() As String = spells(i).Split(" ")
            res.Add(s(0).ToUpper, New AllDataStructues.Spell With {.area = s(4), _
                                                         .castCost = AllDataStructues.Cost.Read(s(3)), _
                                                         .category = s(1), _
                                                         .level = s(2), _
                                                         .name = s(0).ToUpper, _
                                                         .researchCost = New Dictionary(Of String, AllDataStructues.Cost)})
        Next i
        For i As Integer = 1 To UBound(rspells) Step 1
            Dim s() As String = rspells(i).Split(" ")
            res.Item(s(1).ToUpper).researchCost.Add(s(0).ToUpper, AllDataStructues.Cost.Read(s(2)))
        Next i
        Return res
    End Function

    Private Function ReadObjSize() As Dictionary(Of String, Size)
        Dim t() As String = comm.TxtSplit(My.Resources.TestObjectSize)
        ObjectsSize.Clear()
        For i As Integer = 1 To UBound(t) Step 1
            Dim r() As String = t(i).Split(CChar(" "))
            ObjectsSize.Add(r(0).ToUpper, New Size(CInt(r(1)), CInt(r(2))))
        Next i
        Return ObjectsSize
    End Function

End Class
