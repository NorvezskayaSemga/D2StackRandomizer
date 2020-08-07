Imports RandomStackGenerator

Friend Class StartForm

    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector
    Dim comm As New Common
    Dim ObjectsSize As New Dictionary(Of String, Size)

    Private Sub GenMany_click() Handles GenManyButton.Click
        For i As Integer = 1 To 300 Step 1
            GenButton.PerformClick()
            Me.Refresh()
        Next i
        MsgBox("Done")
    End Sub

    Private Sub RndTest() Handles RndTestButton.Click
        Dim r As New RndValueGen
        Dim vanillaR As New OriginalRndGenerator
        Dim maxInt As Integer = Short.MaxValue

        Dim steps As Integer = 10 ^ 7
        Dim result(steps) As Double

        Dim t11 As Integer = Environment.TickCount
        For i As Integer = 0 To steps Step 1
            result(i) = r.PRand(0, 1)
        Next i
        Dim t12 As Integer = Environment.TickCount
        Dim u1 As Double = CalcUniformity(makeDistribution(result))

        Dim t(8 * result.Length - 1) As Byte
        For i As Integer = 0 To steps Step 1
            Dim b() As Byte = BitConverter.GetBytes(result(i))
            For j As Integer = 0 To UBound(b) Step 1
                t(8 * i + j) = b(j)
            Next j
        Next i
        IO.File.WriteAllBytes("./myRandomNumbers.txt", t)
        Console.WriteLine("current rand: u= " & u1 & " t= " & t12 - t11)


        Dim t21 As Integer = Environment.TickCount
        For i As Integer = 0 To steps Step 1
            result(i) = vanillaR.randomNumberUpTo(maxInt) / (maxInt - 1)
        Next i
        Dim t22 As Integer = Environment.TickCount
        Dim u2 As Double = CalcUniformity(makeDistribution(result))

        Console.WriteLine("   test rand: u= " & u2 & " t= " & t22 - t21)
    End Sub
    Private Function makeDistribution(ByVal v() As Double) As Integer()
        Dim dx As Double = 0.01
        Dim result(1 / dx - 1) As Integer
        For i As Integer = 0 To UBound(v) Step 1
            For j As Integer = 0 To UBound(result) Step 1
                If v(i) >= CDbl(j) * dx And v(i) < CDbl(j + 1) * dx Then
                    result(j) += 1
                    Exit For
                End If
            Next j
        Next i
        Return result
    End Function
    Private Function CalcUniformity(ByRef d() As Integer) As Double
        Dim average As Integer
        For j As Integer = 0 To UBound(d) Step 1
            average += d(j)
        Next j
        average /= d.Length
        Dim res As Double = 0
        For j As Integer = 0 To UBound(d) Step 1
            res += Math.Pow(d(j) - average, 2)
        Next j
        Return average / Math.Sqrt(res + 1)
    End Function


    Private Sub GenButton_Click() Handles GenButton.Click

        Call comm.ReadExcludedObjectsList({"%default%"})
        Dim objSizeArray() As ImpenetrableObjects.GlobalMapDecoration = ReadObjSize()

        Dim objplace As New ImpenetrableObjects(objSizeArray, {"%default%"}, {"%default%"}, {"%default%"}, ReadSpells)

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
        sM.xSize = 96
        sM.ySize = 96
        sM.RaceLocsDistTolerance = 0.2
        sM.nRaces = races
        sM.minPassDist = 7
        sM.minPassWidth = 1.1
        sM.AddGuardsBetweenLocations = True
        sM.LocExpRatio = 2
        sM.PassGuardsPowerMultiplicator = 2
        sM.ObjectGuardsPowerMultiplicator = 1.2
        sM.Wealth = 0.8
        sM.WaterAmount = 0
        sM.SpellsMaxLevel = 5
        sM.RoadsAmount = 0
        sM.ForestAmount = 0

        Dim sR As Map.SettingsLoc
        sR.AverageRadius = 20
        sR.maxEccentricityDispersion = 0.15
        sR.maxRadiusDispersion = 0
        sR.maxGoldMines = 1
        sR.maxManaSources = 1
        sR.maxCities = 1
        sR.maxMages = 1
        sR.maxMercenaries = 0
        sR.maxRuins = 3
        sR.maxTrainers = 0
        sR.maxVendors = 0
        sR.minStackToStackDist = 4
        sR.expAmount = 6000

        sR.mageGlobalSpellsEnabled = False
        sR.mageSpellsCount = 5
        sR.mageSpellsMaxLevel = 2
        sR.mageSpellsMinLevel = 1
        sR.merchItemsCost = 2000
        sR.merchMaxItemCost = 1000
        sR.merchMinItemCost = 10
        sR.mercenariesCount = 3
        sR.mercenariesMaxExpBar = 900
        sR.mercenariesMinExpBar = 200

        sR.scaleContent = False

        Dim sC As Map.SettingsLoc
        sC.AverageRadius = 17
        sC.maxEccentricityDispersion = 0.4
        sC.maxRadiusDispersion = 0.3

        sC.maxGoldMines = 1
        sC.maxManaSources = 1
        sC.maxCities = 1
        sC.maxMages = 0
        sC.maxMercenaries = 0
        sC.maxRuins = 3
        sC.maxTrainers = 0
        sC.maxVendors = 0

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

        sC.scaleContent = True


        sM.ApplySymmetry = SymmCheckBox.Checked
        sM.SymmetryClass = -1

        Dim genTimeLimit As Integer = 10000

        If Not UseTemplateCheckBox.Checked Then
            grid = New MapGenWrapper(objplace).SimpleGen(sM, sR, sC, genTimeLimit)
        Else
            Dim sL() As ImpenetrableMeshGen.GenSettings.LocationGenSetting
            If SymmCheckBox.Checked Then
                sC.AverageRadius *= 1.3
                ReDim sL(2)
                sL(0).minValues = sR
                sL(0).maxValues = sR
                sL(0).posX.min = 0.1
                sL(0).posX.max = 0.2
                sL(0).posY.min = 0.1
                sL(0).posY.max = 0.25

                sL(1).minValues = sC
                sL(1).maxValues = sC
                sL(1).posX.min = 0.45
                sL(1).posX.max = 0.55
                sL(1).posY.min = 0.45
                sL(1).posY.max = 0.55

                sL(2).minValues = sC
                sL(2).maxValues = sC
                sL(2).posX.min = 0.01
                sL(2).posX.max = 0.15
                sL(2).posY.min = 0.45
                sL(2).posY.max = 0.55
            Else
                sR.scaleContent = True
                sM.nRaces = 2
                ReDim sL(4)
                sL(0).minValues = sR
                sL(0).maxValues = sR
                sL(0).posX.min = 0.1
                sL(0).posX.max = 0.9
                sL(0).posY.min = 0.1
                sL(0).posY.max = 0.25

                sL(1).minValues = sR
                sL(1).maxValues = sR
                sL(1).posX.min = 0.1
                sL(1).posX.max = 0.9
                sL(1).posY.min = 0.75
                sL(1).posY.max = 0.9

                sL(2).minValues = sC
                sL(2).maxValues = sC
                sL(2).posX.min = 0.45
                sL(2).posX.max = 0.55
                sL(2).posY.min = 0.45
                sL(2).posY.max = 0.55

                sL(3).minValues = sC
                sL(3).maxValues = sC
                sL(3).posX.min = 0.01
                sL(3).posX.max = 0.15
                sL(3).posY.min = 0.45
                sL(3).posY.max = 0.55

                sL(4).minValues = sC
                sL(4).maxValues = sC
                sL(4).posX.min = 0.85
                sL(4).posX.max = 0.99
                sL(4).posY.min = 0.45
                sL(4).posY.max = 0.55
            End If
            For i As Integer = 0 To UBound(sL) Step 1
                sL(i).minValues.AverageRadius *= 0.9
                sL(i).maxValues.AverageRadius *= 1.1
                sL(i).minValues.maxGoldMines *= 0.9
                sL(i).maxValues.maxGoldMines *= 1.1
            Next i
            grid = New MapGenWrapper(objplace).TemplateGen(sM, sL, genTimeLimit)
        End If

        'Dim randstack As New RandStack(ReadTestUnits, ReadTestItems, {"%default%"}, {"%default%"}, True)
        'Dim isnew As Boolean = True
        'For Each v As AllDataStructues.DesiredStats In grid.groupStats.Values
        '    Dim stack As AllDataStructues.Stack = randstack.Gen(v, False, False)
        '    Call names.GenName(stack, randstack, isnew)
        '    isnew = False
        '    Console.WriteLine(stack.name)
        'Next v

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
                    Dim objname As String = grid.board(x, y).objectName
                    If grid.board(x, y).objectID = DefMapObjects.Types.Mine Then
                        x = x
                    End If
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
                    't(x, y) = grid.board(x, y).objRace.Item(0)
                    t(x, y) = grid.board(x, y).locID(0)
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

    Private Function ReadSpells() As AllDataStructues.Spell()
        Dim spells() As String = comm.TxtSplit(My.Resources.TestSpells)
        Dim rspells() As String = comm.TxtSplit(My.Resources.TestSpellsRace)
        Dim res(UBound(spells) - 1) As AllDataStructues.Spell
        For i As Integer = 1 To UBound(spells) Step 1
            Dim s() As String = spells(i).Split(" ")
            res(i - 1) = New AllDataStructues.Spell With {.area = s(4), _
                                                          .castCost = AllDataStructues.Cost.Read(s(3)), _
                                                          .category = s(1), _
                                                          .level = s(2), _
                                                          .spellID = s(0).ToUpper, _
                                                          .name = s(0) & "_test", _
                                                          .researchCost = New Dictionary(Of String, AllDataStructues.Cost)}
        Next i
        For i As Integer = 1 To UBound(rspells) Step 1
            Dim s() As String = rspells(i).Split(" ")
            For j As Integer = 0 To UBound(res) Step 1
                If res(j).spellID.ToUpper = s(1).ToUpper Then
                    res(j).researchCost.Add(s(0).ToUpper, AllDataStructues.Cost.Read(s(2)))
                    Exit For
                End If
            Next j
        Next i
        Return res
    End Function

    Private Function ReadObjSize() As ImpenetrableObjects.GlobalMapDecoration()
        Dim t() As String = comm.TxtSplit(My.Resources.TestObjectSize)
        Dim result(UBound(t) - 1) As ImpenetrableObjects.GlobalMapDecoration
        ObjectsSize.Clear()
        For i As Integer = 1 To UBound(t) Step 1
            Dim r() As String = t(i).Split(CChar(" "))
            ObjectsSize.Add(r(0).ToUpper, New Size(CInt(r(1)), CInt(r(2))))
            result(i - 1) = New ImpenetrableObjects.GlobalMapDecoration With {.ID = r(0).ToUpper, .Size = New Size(CInt(r(1)), CInt(r(2)))}
        Next i
        Return result
    End Function

    Private Function ReadTestUnits() As AllDataStructues.Unit()
        Dim comm As New Common
        Dim s() As String = comm.TxtSplit(My.Resources.TestUnitsTable)
        Dim r() As String
        Dim UnitsList(UBound(s) - 1) As AllDataStructues.Unit
        For i As Integer = 1 To UBound(s) Step 1
            r = s(i).Split(" ")
            If r.Length = 12 Then
                Do While Not r(0).Substring(0, 1).ToLower = "g" And r(0).Length > 1
                    r(0) = r(0).Substring(1)
                Loop
                UnitsList(i - 1).unitID = r(0)
                UnitsList(i - 1).level = r(2)
                UnitsList(i - 1).race = r(3)
                UnitsList(i - 1).unitBranch = r(4)
                UnitsList(i - 1).small = r(5)
                UnitsList(i - 1).EXPkilled = r(7)
                UnitsList(i - 1).EXPnext = r(8)
                UnitsList(i - 1).leadership = r(9)
                UnitsList(i - 1).waterOnly = r(10)
                UnitsList(i - 1).reach = r(11)
                UnitsList(i - 1).name = UnitsList(i - 1).unitID & "_test"
            End If
        Next i
        Return UnitsList
    End Function

    Private Function ReadTestItems() As AllDataStructues.Item()
        Dim comm As New Common
        Dim s() As String = comm.TxtSplit(My.Resources.TestItemsTable)
        Dim r() As String
        Dim ItemsList(UBound(s) - 1) As AllDataStructues.Item
        For i As Integer = 1 To UBound(s) Step 1
            r = s(i).Split(" ")
            If r.Length = 3 Then
                ItemsList(i - 1).type = r(0)
                ItemsList(i - 1).itemID = r(1)
                ItemsList(i - 1).name = r(1) & "_test"
                ItemsList(i - 1).itemCost = AllDataStructues.Cost.Read(r(2))
            End If
        Next i
        Return ItemsList
    End Function

    Private Sub itemGenTest()

        Dim d As New GenDefaultValues(Nothing)
        Dim r As New RandStack(ReadTestUnits, ReadTestItems, {d.wReadDefaultFileKeyword}, {d.wReadDefaultFileKeyword}, {d.wReadDefaultFileKeyword}, {d.wReadDefaultFileKeyword}, {d.wReadDefaultFileKeyword})

        Dim items As New List(Of String)
        items.Add("G000IG0004")

        Dim lcost As AllDataStructues.Cost = r.LootCost(items)
        Dim igen As AllDataStructues.LootGenSettings = r.GetItemsGenSettings(items)
        Dim result As List(Of String) = r.ItemsGen(AllDataStructues.Cost.Sum(lcost), igen, Nothing, New Point(0, 0))
    End Sub

End Class
