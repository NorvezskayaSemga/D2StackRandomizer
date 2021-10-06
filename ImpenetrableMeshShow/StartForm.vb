Imports RandomStackGenerator

Friend Class StartForm

    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector
    Dim comm As New Common(GenDefaultValues.DefaultMod)
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
        'Dim vanillaR As New OriginalRndGenerator
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
            'result(i) = vanillaR.randomNumberUpTo(maxInt) / (maxInt - 1)
            result(i) = r.RndDblFast(0, 1)
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
        'Call StackLocationsGen.PassageGuardPlacer.speedBanchmark()
        'Call ImpenetrableMeshGen.ActiveObjectsPlacer.speedBanchmark()

        Dim tf As New TemplateForge(RandomStackGenerator.GenDefaultValues.TextLanguage.Rus)
        tf.ReadCommonMapSettingsFromFile(".\Resources", "example_template_2_unsymm.txt")

        For Each lang As GenDefaultValues.TextLanguage In System.Enum.GetValues(GetType(GenDefaultValues.TextLanguage))
            Call TemplateForge.GetPermissibleParametersRange(lang)
        Next lang

        Dim param() As TemplateForge.Parameter = TemplateForge.GetPermissibleParametersRange(GenDefaultValues.TextLanguage.Rus)

        Dim treesAmount() As Integer = {0, 20, 20, 20, 20, 20, 0, 0, 0, 0, 0, 0, 0, 0, 20}

        Dim rstack As New RandStack(DefaultGenData)
        Dim objCont As New ObjectsContentSet(rstack)

        'Dim ds As New AllDataStructues.DesiredStats With {.ExpBarAverage = 50, .ExpStackKilled = 56, .MaxGiants = 3, _
        '                                                  .MeleeCount = 3, .Race = New List(Of Integer), .StackSize = 6}
        'ds.Race.Add(16)
        'Dim dds As AllDataStructues.DesiredStats = AllDataStructues.DesiredStats.Copy(ds)
        'Call rstack.GenFingters(ds, dds, 3, -1, True, 6, True, New List(Of Integer), 0, 0.5, -1)

        Call comm.ReadExcludedObjectsList(False)
        Dim objSizeArray() As ImpenetrableObjects.GlobalMapDecoration = ReadObjSize()

        Dim objplace As New ImpenetrableObjects(objSizeArray, False, ReadSpells, comm)

        Dim grid As Map
        Dim genTimeLimit As Integer = 10000

        Dim path As String = ".\Resources\"

        If True Then
            path &= "template_48x48_unsymm_simple.txt"
        Else
            If Not UseTemplateCheckBox.Checked Then
                path &= "example_template_1.txt"
            Else
                If SymmCheckBox.Checked Then
                    path &= "example_template_2_symm.txt"
                Else
                    path &= "example_template_2_unsymm.txt"
                End If
            End If
        End If

        Dim gsettings As ImpenetrableMeshGen.GenSettings = ImpenetrableMeshGen.GenSettings.Read(path)

        '### можно внести какие-то ихменения в шаблон типа размера карты, количества леса/воды и т.д.
        If Not UseTemplateCheckBox.Checked Then
            Dim races As Integer
            If sel1.Checked Then
                races = 2
            ElseIf sel2.Checked Then
                races = 3
            Else
                races = 4
            End If
            gsettings.common_settMap.nRaces = races
            gsettings.common_settMap.ApplySymmetry = SymmCheckBox.Checked
            gsettings.common_settMap.SymmetryClass = -1
        End If
        'gsettings.common_settMap.RoadsAmount = 0
        'gsettings.common_settMap.WaterAmount = 0
        'gsettings.common_settMap.ForestAmount = 0
        '###

        grid = New MapGenWrapper(objplace).CommonGen(gsettings, genTimeLimit, GenDefaultValues.DefaultMod)

        If Not IsNothing(grid.board) Then
            Call ShowResult(grid)
            Call shortMapFormat.MapConversion(grid, gsettings, objSizeArray, objCont, True, True, treesAmount, GenDefaultValues.TextLanguage.Rus)
        End If

        Console.WriteLine(grid.log.PrintAll)
        LogTextBox.Text = grid.log.PrintAll
    End Sub
    Public Sub ShowResult(ByRef grid As Map)
        Dim t(grid.xSize, grid.ySize) As Integer
        For x As Integer = 0 To grid.xSize Step 1
            For y As Integer = 0 To grid.ySize Step 1
                If grid.board(x, y).locID.Count = 0 Or Not grid.board(x, y).passability.isBorder Then
                    t(x, y) = 0
                End If
                If grid.board(x, y).passability.isAttended Then
                    t(x, y) = 51 + 2 * grid.board(x, y).mapObject.objectID
                ElseIf grid.board(x, y).passability.isPass Then
                    t(x, y) = 155
                End If
                If grid.board(x, y).passability.isPenetrable Then
                    't(x, y) = 100
                End If
                If grid.board(x, y).stack.GuardLoc Then
                    t(x, y) = 185
                ElseIf grid.board(x, y).mapObject.objectID = DefMapObjects.Types.City Then
                    t(x, y) += 55
                End If
                If grid.board(x, y).stack.PassGuardLoc Then
                    t(x, y) = 94
                End If
                If grid.board(x, y).passability.isBorder Then
                    If Not IsNothing(grid.board(x, y).mapObject.objRace) Then
                        t(x, y) = grid.board(x, y).mapObject.objRace.Item(0)
                    Else
                        t(x, y) = 5 + grid.board(x, y).locID(0)
                    End If
                ElseIf grid.board(x, y).surface.isWater Then
                    't(x, y) += 200
                End If
                If grid.board(x, y).surface.isForest Then
                    't(x, y) = 135
                End If
                If grid.board(x, y).surface.isRoad Then
                    't(x, y) = 145
                End If
            Next y
        Next x
        For x As Integer = 0 To grid.xSize Step 1
            For y As Integer = 0 To grid.ySize Step 1
                If grid.board(x, y).passability.isBorder Then
                    If Not IsNothing(grid.board(x, y).mapObject.objectName) AndAlso grid.board(x, y).mapObject.objectName.Length > 0 Then
                        Dim s As Size = ObjectsSize.Item(grid.board(x, y).mapObject.objectName)
                        For j As Integer = y To y + s.Height - 1 Step 1
                            For i As Integer = x To x + s.Width - 1 Step 1
                                If Not grid.board(i, j).passability.isBorder Or t(i, j) > 500 Then
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

    Public Shared Function ReadSpells() As AllDataStructues.Spell()
        Dim spells() As String = ValueConverter.TxtSplit(My.Resources.TestSpells)
        Dim rspells() As String = ValueConverter.TxtSplit(My.Resources.TestSpellsRace)
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

    Public Function ReadObjSize() As ImpenetrableObjects.GlobalMapDecoration()
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

    Public Shared Function ReadTestUnits(ByVal modName As String) As AllDataStructues.Unit()
        Dim comm As New Common(modName)
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

    Public Shared Function ReadTestItems(ByVal modName As String) As AllDataStructues.Item()
        Dim comm As New Common(modName)
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

    Private Sub itemGenTest(ByVal modName As String)

        Dim r As New RandStack(DefaultGenData)

        Dim items As New List(Of String)
        items.Add("G000IG0004")

        Dim lcost As AllDataStructues.Cost = r.LootCost(items)
        Dim igen As AllDataStructues.LootGenSettings = r.GetItemsGenSettings(items, False)
        Dim sett As New AllDataStructues.CommonLootCreationSettings With {.GoldCost = AllDataStructues.Cost.Sum(lcost), _
                                                                          .IGen = igen, _
                                                                          .pos = New Point(0, 0), _
                                                                          .TypeCostRestriction = Nothing, _
                                                                          .ApplyStrictTypesFilter = True}
        Dim result As List(Of String) = r.ItemsGen(sett)
    End Sub

    Public Shared Function DefaultGenData() As RandStack.ConstructorInput
        Dim r As New RandStack.ConstructorInput
        r.AllUnitsList = ReadTestUnits(GenDefaultValues.DefaultMod)
        r.AllItemsList = ReadTestItems(GenDefaultValues.DefaultMod)
        r.AllSpellsList = ReadSpells()
        r.modName = GenDefaultValues.DefaultMod
        Return r
    End Function

End Class
