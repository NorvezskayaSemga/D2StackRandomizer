Imports RandomStackGenerator

Public Class StartForm

    Dim genmesh As New ImpenetrableMeshGen
    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector
    Dim symm As New SymmetryOperations
    Dim stackstats As New StackPowerGen
    Dim racegen As New RaceGen

    Private Sub GenButton_Click() Handles GenButton.Click

        Dim grid As Map
        Dim races As Integer
        If sel1.Checked Then
            races = 2
        ElseIf sel2.Checked Then
            races = 3
        Else
            races = 4
        End If

        Dim sM As ImpenetrableMeshGen.SettingsMap
        sM.xSize = 95
        sM.ySize = 95
        sM.RaceLocsDistTolerance = 0.2
        sM.nRaces = races
        sM.minPassDist = 7
        sM.minPassWidth = 1.1
        sM.AddGuardsBetweenLocations = True
        sM.LocExpRatio = 2
        sM.PassGuardsPowerMultiplicator = 2

        Dim sR As ImpenetrableMeshGen.SettingsLoc
        sR.AverageRadius = 20
        sR.maxEccentricityDispersion = 0.15
        sR.maxRadiusDispersion = 0
        sR.maxGoldMines = 2
        sR.maxManaSources = 3
        sR.maxCities = 0
        sR.maxMages = 1
        sR.maxMercenaries = 0
        sR.maxRuins = 2
        sR.maxTrainers = 0
        sR.maxVendors = 1
        sR.minStackToStackDist = 4
        sR.expAmount = 3000

        Dim sC As ImpenetrableMeshGen.SettingsLoc
        sC.AverageRadius = 15
        sC.maxEccentricityDispersion = 0.4
        sC.maxRadiusDispersion = 0.3

        sC.maxGoldMines = 1.6
        sC.maxManaSources = 1.6
        sC.maxCities = 0.3
        sC.maxMages = 0.1
        sC.maxMercenaries = 0.1
        sC.maxRuins = 2
        sC.maxTrainers = 0
        sC.maxVendors = 0.2

        sC.minStackToStackDist = 5
        sC.expAmount = 2000

        Dim gt As Integer = 5000
again:
        If Not SymmCheckBox.Checked Then
            grid = genmesh.UnsymmGen(sM, sR, sC, gt)
        Else
            grid = genmesh.SymmGen(sM, sR, sC, gt)
        End If

        If Not genmesh.TestMap(grid, False) Then Exit Sub

        If Not IsNothing(grid) Then
            Dim staclocgen As New StackLocationsGen
            If Not staclocgen.Gen(grid, sM, sR, sC, gt) Then GoTo again
        Else
            Exit Sub
        End If

        If Not genmesh.TestMap(grid, True) Then Exit Sub

        Dim ststats As Dictionary(Of Integer, RandStack.DesiredStats) = stackstats.Gen(grid, sM, sR, sC)
        Call racegen.Gen(grid, ststats, Nothing)

        'запоминаем набор точек с наибольшим n
        'произвед 1/r - стат вес для nearwith = -1

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
                End If
            Next y
        Next x
        Dim mult As Integer = zoom.CalcMultiplicator(Math.Max(UBound(t, 1), UBound(t, 2)) + 1)
        Dim mgrid(,) As Integer = zoom.Zoom(t, mult)
        Dim c(,) As Color = draw.MakeIslandsColorMap(mgrid)
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

End Class
