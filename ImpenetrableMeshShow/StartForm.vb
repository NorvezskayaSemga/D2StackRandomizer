Imports RandomStackGenerator

Friend Class StartForm

    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector
    Dim comm As New Common(GenDefaultValues.DefaultMod)

    Private Sub GenMany_click() Handles GenManyButton.Click
        Dim startTime As Integer = Environment.TickCount
        For i As Integer = 1 To 300 Step 1
            GenButton.PerformClick()
            Me.Refresh()
        Next i
        Dim endTime As Integer = Environment.TickCount
        MsgBox("Done. " & Math.Round((endTime - startTime) / 60, 0) & " minutes")
    End Sub

    Private Sub GenButton_Click() Handles GenButton.Click
        'Call StackLocationsGen.PassageGuardPlacer.speedBanchmark()
        'Call ImpenetrableMeshGen.ActiveObjectsPlacer.speedBanchmark()

        'Call Tests.LootRegenerator()
        'Call Tests.SingleItemRegenerator()

        If GenDefaultValues.writeToConsole Then
            Dim tf As New TemplateForge(RandomStackGenerator.GenDefaultValues.TextLanguage.Rus)
            tf.ReadCommonMapSettingsFromFile(".\Resources", "example_template_2_unsymm.txt")

            For Each lang As GenDefaultValues.TextLanguage In System.Enum.GetValues(GetType(GenDefaultValues.TextLanguage))
                Call TemplateForge.GetPermissibleParametersRange(lang)
            Next lang
            'Call TemplateForge.GetPermissibleParametersRange(GenDefaultValues.TextLanguage.Eng)
            Dim param() As TemplateForge.Parameter = TemplateForge.GetPermissibleParametersRange(GenDefaultValues.TextLanguage.Rus)
        End If

        Dim rstack As New RandStack(TestDataRead.DefaultGenData)
        Dim objCont As New ObjectsContentSet(rstack)

        'Dim ds As New AllDataStructues.DesiredStats With {.ExpBarAverage = 50, .ExpStackKilled = 56, .MaxGiants = 3, _
        '                                                  .MeleeCount = 3, .Race = New List(Of Integer), .StackSize = 6}
        'ds.Race.Add(16)
        'Dim dds As AllDataStructues.DesiredStats = AllDataStructues.DesiredStats.Copy(ds)
        'Call rstack.GenFingters(ds, dds, 3, -1, True, 6, True, New List(Of Integer), 0, 0.5, -1)

        comm.excludedObjects = rstack.comm.excludedObjects
        Dim gm As New NevendaarTools.GameModel
        gm.Load(".\Resources", True)

        'Call CheckModifiersUse(gm)
        'Call CheckAttacksUse(gm)

        Dim objplace As New ImpenetrableObjects(gm, False, comm)

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
        gsettings.common_settMap.xSize = 50
        gsettings.common_settMap.ySize = 50

        Dim startTime As Integer = Environment.TickCount
        grid = New MapGenWrapper(objplace).CommonGen(gsettings, genTimeLimit, GenDefaultValues.DefaultMod)

        If Not IsNothing(grid.board) Then
            Call ShowResult(grid, gm)
            Call shortMapFormat.MapConversion(grid, gsettings, gm, objCont, True, True, GenDefaultValues.TextLanguage.Rus)
        End If
        Dim endTime As Integer = Environment.TickCount
        Console.WriteLine(endTime - startTime & " ms")

        If GenDefaultValues.writeToConsole Then Console.WriteLine(grid.log.PrintAll)
        If GenDefaultValues.writeToLog Then LogTextBox.Text = grid.log.PrintAll
    End Sub
    Public Sub ShowResult(ByRef grid As Map, ByRef gm As NevendaarTools.GameModel)
        Dim ObjectsSize As Dictionary(Of String, Size) = AllDataStructues.MapObjectInfo.getSizeDictionary(gm, comm)
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

    'Private Function ReadObjSize() As ImpenetrableObjects.GlobalMapDecoration()
    '    Dim t() As String = ValueConverter.TxtSplit(My.Resources.TestObjectSize)
    '    Dim result(UBound(t) - 1) As ImpenetrableObjects.GlobalMapDecoration
    '    ObjectsSize.Clear()
    '    For i As Integer = 1 To UBound(t) Step 1
    '        Dim r() As String = t(i).Split(CChar(" "))
    '        ObjectsSize.Add(r(0).ToUpper, New Size(CInt(r(1)), CInt(r(2))))
    '        result(i - 1) = New ImpenetrableObjects.GlobalMapDecoration With {.ID = r(0).ToUpper, .Size = New Size(CInt(r(1)), CInt(r(2)))}
    '    Next i
    '    Return result
    'End Function

    Private Sub CheckModifiersUse(ByRef gameModel As NevendaarTools.GameModel)
        Dim items As List(Of NevendaarTools.GItem) = gameModel.GetAllT(Of NevendaarTools.GItem)()
        Dim spells As List(Of NevendaarTools.Gspell) = gameModel.GetAllT(Of NevendaarTools.Gspell)()
        Dim attacks As List(Of NevendaarTools.Gattack) = gameModel.GetAllT(Of NevendaarTools.Gattack)()
        Dim lupgr As List(Of NevendaarTools.GleaUpg) = gameModel.GetAllT(Of NevendaarTools.GleaUpg)()

        Dim imods, smods, amods, umods As New List(Of String)
        For Each i As NevendaarTools.Gspell In spells
            If Not IsNothing(i.modif_id.key) AndAlso Not i.modif_id.key.ToLower = "g000000000" Then
                If smods.Contains(i.modif_id.key.ToLower) Then
                    Console.WriteLine("spells " & i.modif_id.key.ToLower & " " & i.name_txt.value.text)
                Else
                    smods.Add(i.modif_id.key.ToLower)
                End If
            End If
        Next i
        For Each i As NevendaarTools.GItem In items
            If Not IsNothing(i.mod_equip.key) AndAlso Not i.mod_equip.key.ToLower = "g000000000" Then
                If imods.Contains(i.mod_equip.key.ToLower) Then
                    Console.WriteLine("equip " & i.mod_equip.key.ToLower & " " & i.name_txt.value.text)
                Else
                    imods.Add(i.mod_equip.key.ToLower)
                End If
            End If
            If Not IsNothing(i.mod_potion.key) AndAlso Not i.mod_potion.key.ToLower = "g000000000" Then
                If imods.Contains(i.mod_potion.key.ToLower) Then
                    Console.WriteLine("potions " & i.mod_potion.key.ToLower & " " & i.name_txt.value.text)
                Else
                    imods.Add(i.mod_potion.key.ToLower)
                End If
            End If
        Next i
        For Each i As NevendaarTools.Gattack In attacks
            For Each w As NevendaarTools.StringLink(Of NevendaarTools.GmodifL) In {i.ward1, i.ward2, i.ward3, i.ward4}
                If Not IsNothing(w.key) AndAlso Not w.key.ToLower = "g000000000" Then
                    If amods.Contains(w.key.ToLower) Then
                        Console.WriteLine("attacks " & w.key.ToLower & " " & i.name_txt.value.text)
                    Else
                        amods.Add(w.key.ToLower)
                    End If
                End If
            Next w
        Next i
        For Each i As NevendaarTools.GleaUpg In lupgr
            If Not IsNothing(i.modif_id) Then
                If Not umods.Contains(i.modif_id.ToString.ToLower) Then
                    umods.Add(i.modif_id.ToString.ToLower)
                End If
            End If
        Next i

        Dim L() As List(Of String) = {umods, imods, amods, smods}
        For i As Integer = 0 To UBound(L) - 1
            For j As Integer = i + 1 To UBound(L)
                For Each t As String In L(i)
                    If L(j).Contains(t) Then
                        Throw New Exception
                    End If
                Next
            Next
        Next

    End Sub
    Private Sub CheckAttacksUse(ByRef gameModel As NevendaarTools.GameModel)
        Dim items As List(Of NevendaarTools.GItem) = gameModel.GetAllT(Of NevendaarTools.GItem)()
        Dim units As List(Of NevendaarTools.Gunit) = gameModel.GetAllT(Of NevendaarTools.Gunit)()

        Dim iatt, uatt As New List(Of String)
        For Each i As NevendaarTools.GItem In items
            If Not IsNothing(i.attack_id.key) AndAlso Not i.attack_id.key.ToLower = "g000000000" Then
                If iatt.Contains(i.attack_id.key.ToLower) Then
                    Console.WriteLine("att items " & i.attack_id.key.ToLower)
                Else
                    iatt.Add(i.attack_id.key.ToLower)
                End If
            End If
        Next i
        For Each i As NevendaarTools.Gunit In units
            If i.unit_cat.key = 0 Then
                For Each a As NevendaarTools.StringLink(Of NevendaarTools.Gattack) In {i.attack_id, i.attack2_id}
                    If Not IsNothing(a.key) AndAlso Not a.key.ToLower = "g000000000" Then
                        If uatt.Contains(a.key.ToLower) Then
                            Console.WriteLine("att units " & a.key.ToLower & " " & a.value.name_txt.value.text & " " & i.name_txt.value.text)
                        Else
                            uatt.Add(a.key.ToLower)
                        End If
                    End If
                Next
            End If
        Next i

        Dim L() As List(Of String) = {iatt, uatt}
        For i As Integer = 0 To UBound(L) - 1
            For j As Integer = i + 1 To UBound(L)
                For Each t As String In L(i)
                    If L(j).Contains(t) Then
                        Throw New Exception
                    End If
                Next
            Next
        Next

    End Sub

End Class

Public Class TestDataRead

    Private Shared Function TXTSplit(ByRef content As String, ByVal expectedLen As Integer) As String()()
        Dim s() As String = ValueConverter.TxtSplit(content)
        Dim r(UBound(s))() As String
        For i As Integer = 0 To UBound(s) Step 1
            r(i) = s(i).Split(" ")
            If expectedLen > -1 And Not r(i).Length = expectedLen Then
                Throw New Exception
            End If
        Next i
        Return r
    End Function

    Public Shared Function DefaultGenData() As RandStack.ConstructorInput
        Dim gm As New NevendaarTools.GameModel
        gm.Load(".\Resources", True)
        Dim r As New RandStack.ConstructorInput
        r.gameModel = gm
        r.settings.modName = GenDefaultValues.DefaultMod
        Return r
    End Function

    Public Shared Function ReadTestBags() As List(Of String)()
        Return ReadBags(False)
    End Function
    Public Shared Function ReadTestRuinsTreasure() As List(Of String)()
        Return ReadBags(True)
    End Function
    Private Shared Function ReadBags(ByVal firstItemOnly As Boolean) As List(Of String)()
        Dim bags()() As String = TXTSplit(My.Resources.TestBags, -1)
        Dim r(UBound(bags)) As List(Of String)
        Dim n As Integer
        For i As Integer = 0 To UBound(bags) Step 1
            r(i) = New List(Of String)
            If firstItemOnly Then
                n = 0
            Else
                n = UBound(bags(i))
            End If
            For j As Integer = 0 To n Step 1
                r(i).Add(bags(i)(j))
            Next j
        Next i
        Return r
    End Function

End Class

Class Tests

    Public Shared Sub RndTest()
        Dim r As New RndValueGen
        'Dim vanillaR As New OriginalRndGenerator
        Dim maxInt As Integer = Short.MaxValue

        Dim steps As Integer = 10 ^ 7
        Dim result(steps) As Double

        Dim t11 As Integer = Environment.TickCount
        For i As Integer = 0 To steps Step 1
            result(i) = r.RndDbl(0, 1)
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
    End Sub
    Private Shared Function makeDistribution(ByVal v() As Double) As Integer()
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
    Private Shared Function CalcUniformity(ByRef d() As Integer) As Double
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

    Public Shared Sub itemGenTest(ByVal modName As String)
        Dim r As New RandStack(TestDataRead.DefaultGenData)

        Dim items As New List(Of String)
        items.Add("G000IG0004")

        Dim lcost As AllDataStructues.Cost = r.LootCost(items)
        Dim igen As AllDataStructues.LootGenSettings = r.GetItemsGenSettings(items, False)
        Dim sett As New AllDataStructues.CommonLootCreationSettings With {.GoldCost = AllDataStructues.Cost.Sum(lcost), _
                                                                          .IGen = igen, _
                                                                          .pos = New Point(0, 0), _
                                                                          .TypeCostRestriction = Nothing}
        Dim result As List(Of String) = r.ItemsGen(sett)
    End Sub

    Private Const minItemGoldCost As Integer = 150
    Public Shared Sub ItemsGenTest_time()
        Dim lootTime, mercenariesTime, merchantTime, mageTime As Integer

        lootTime = ItemsGenTest() '12761
        mercenariesTime = MakeMercenariesListTest() '18073
        merchantTime = MakeMerchItemsListTest() '13701
        mageTime = MakeSpellsListTest() '25538

        Console.WriteLine("'" & lootTime)
        Console.WriteLine("'" & mercenariesTime)
        Console.WriteLine("'" & merchantTime)
        Console.WriteLine("'" & mageTime)

        '13136
        '17784
        '11279
        '42807

        Throw New Exception("")
    End Sub
    Private Shared Function CreateRandStack() As RandStack
        Dim gm As New NevendaarTools.GameModel
        gm.Load("./Resources", True)
        Dim rstackData As New RandStack.ConstructorInput With {.gameModel = gm}
        rstackData.settings.modName = GenDefaultValues.DefaultMod
        rstackData.mapData.capitalPos = {New Point(1000, 1000)}
        Return New RandStack(rstackData)
    End Function
    Private Shared Function ItemsGenTest() As Integer
        Dim target As RandStack = CreateRandStack()
        target.log.Disable()

        Dim t As Integer = Environment.TickCount
        Dim inputItems() As List(Of String)
        Dim items As List(Of String)
        Dim testRuinsLootCreator As Boolean
        For ruinsLoot As Integer = 0 To 1 Step 1
            testRuinsLootCreator = (ruinsLoot = 1)
            If testRuinsLootCreator Then
                inputItems = ImpenetrableMeshShow.TestDataRead.ReadTestRuinsTreasure
            Else
                inputItems = ImpenetrableMeshShow.TestDataRead.ReadTestBags
            End If
            For p As Integer = 1 To 10 Step 1
                For strictFilter As Integer = 0 To 1 Step 1
                    target.settings.applyStrictTypesFilter = (strictFilter = 1)
                    For i As Integer = 0 To UBound(inputItems) Step 1
                        Call target.ResetAddedItems()
                        Call target.ResetItemWeightMultiplier()
                        items = RunLootCreation(target, inputItems(i), testRuinsLootCreator)
                    Next i
                Next strictFilter
            Next p
        Next
        Return Environment.TickCount - t
    End Function
    Private Shared Function RunLootCreation(ByRef target As RandStack, ByRef inputLoot As List(Of String), ByRef ruinsTest As Boolean) As List(Of String)
        Dim igen As AllDataStructues.LootGenSettings = target.GetItemsGenSettings(inputLoot, ruinsTest)
        Dim settings As New AllDataStructues.CommonLootCreationSettings
        settings.GoldCost = AllDataStructues.Cost.Sum(target.LootCost(inputLoot))
        settings.IGen = igen
        settings.pos = New Point(10, 10)
        settings.TypeCostRestriction = Nothing
        Dim result As List(Of String)
        If ruinsTest Then
            result = New List(Of String)
            result.Add(target.ThingGen(settings))
        Else
            result = target.ItemsGen(settings)
        End If
        Return result
    End Function

    Private Shared Function MakeSpellsListTest() As Integer

        Dim rStack As RandStack = CreateRandStack()

        Dim target As New ObjectsContentSet(rStack)
        Dim ok As Boolean = True
        Dim actual As List(Of String)
        Dim input, mines As New List(Of String)
        Dim races() As String = New String() {"H", "U", "C", "R"}
        Dim mass() As String = New String() {"F", "T"}
        Dim c As New AllDataStructues.Cost

        Dim t As Integer = Environment.TickCount
        For m1 As Integer = 0 To 1 Step 1
            c.Black = m1
            c.Blue = m1
            For m3 As Integer = 0 To 1 Step 1
                c.Green = m3
                c.Red = m3
                For m5 As Integer = 0 To 1 Step 1
                    c.White = m5
                    mines.Clear()
                    If c.Black > 0 Then mines.Add("G000CR0000RG")
                    If c.Blue > 0 Then mines.Add("G000CR0000YE")
                    If c.Green > 0 Then mines.Add("G000CR0000GR")
                    If c.Red > 0 Then mines.Add("G000CR0000RD")
                    If c.White > 0 Then mines.Add("G000CR0000WH")
                    For Each g As String In mass
                        For Each r As String In races
                            For level As Integer = 1 To 5 Step 2
                                If input.Count > 10 Then input.Clear()
                                input.Add(level & r & g)
                                actual = target.MakeSpellsList(New AllDataStructues.DesiredStats With {.shopContent = input}, mines, rStack.log, -1)
                                If actual.Count = 0 Then ok = False
                            Next level
                        Next r
                    Next g
                Next
            Next
        Next
        Dim dt As Integer = Environment.TickCount - t
        Return dt
    End Function
    Private Shared Function MakeMercenariesListTest() As Integer

        Dim rStack As RandStack = CreateRandStack()

        Dim target As New ObjectsContentSet(rStack)
        Dim actual As List(Of String)
        Dim input As New List(Of String)

        Dim t As Integer = Environment.TickCount
        For i As Integer = 100 To 10000 Step 100
            'input.Clear()
            input.Add(i)
            actual = target.MakeMercenariesList(New AllDataStructues.DesiredStats With {.shopContent = input}, rStack.log, -1)
        Next i
        Return Environment.TickCount - t
    End Function
    Private Shared Function MakeMerchItemsListTest() As Integer
        Dim rStack As RandStack = CreateRandStack()
        Dim target As New ObjectsContentSet(rStack)
        Dim actual As List(Of String)
        Dim input As New List(Of String)

        Dim t As Integer = Environment.TickCount
        For p As Integer = 1 To 10 Step 1
            input.Clear()
            For i As Integer = 100 To 10000 Step 100
                'input.Clear()
                input.Add(i)
                actual = target.MakeMerchantItemsList(New AllDataStructues.DesiredStats With {.shopContent = input, .IGen = New AllDataStructues.LootGenSettings(False)}, Nothing, rStack.log, -1)
            Next i
        Next p
        Return Environment.TickCount - t
    End Function

    Public Shared Sub ModificatorsSpeedTest()

        Dim r As New RandStack(TestDataRead.DefaultGenData)

        Dim commonModificators() As String = _
            {"g000um2002", "g000um2002", "g000um2101", "g000um4017", "g000um4017", "g000um4017", _
             "g000um7545", "g000um7545", "g100um0030", "g100um0030", "g100um0030", "g200um0018"}
        Dim wards() As String = {"g100um0002", "g000um9014"}
        Dim damage() As String = {"g002um0070", "g002um0071", "g005um0187", "g005um0187"}

        Dim testStack As New AllDataStructues.Stack
        ReDim testStack.units(5)
        testStack.units(0) = New AllDataStructues.Stack.UnitInfo("g000uu0017", 5, New List(Of String), r) 'хил
        testStack.units(1) = New AllDataStructues.Stack.UnitInfo("g000uu0151", 5, New List(Of String), r) 'хил
        testStack.units(2) = New AllDataStructues.Stack.UnitInfo("g000uu8218", 5, New List(Of String), r) 'хил
        testStack.units(3) = New AllDataStructues.Stack.UnitInfo("g000uu7587", 5, New List(Of String), r) 'урон
        testStack.units(4) = New AllDataStructues.Stack.UnitInfo("g000uu0053", 5, New List(Of String), r) 'урон
        testStack.units(5) = New AllDataStructues.Stack.UnitInfo("g000uu8021", 5, New List(Of String), r) 'урон

        For i As Integer = 0 To UBound(testStack.units) Step 1
            testStack.units(i).modificators.AddRange(commonModificators)
        Next i
        For i As Integer = 1 To UBound(testStack.units) - 1 Step 1
            testStack.units(i).modificators.AddRange(wards)
        Next i
        For i As Integer = 3 To UBound(testStack.units) Step 1
            testStack.units(i).modificators.AddRange(damage)
        Next i

        Dim stats As AllDataStructues.DesiredStats = r.StackStats(testStack, False)
        Dim genSettings As New AllDataStructues.CommonStackCreationSettings
        genSettings.StackStats = stats

        Dim s As New AllDataStructues.Stack
        ReDim s.units(UBound(testStack.units))

        Dim t0 As Integer = Environment.TickCount
        For k As Integer = 0 To 1000 Step 1
            For i As Integer = 0 To UBound(testStack.units) Step 1
                s.units(i) = AllDataStructues.Stack.UnitInfo.Copy(testStack.units(i))
                s.units(i).modificators.Clear()
            Next i
            Call New RandStack.RecursiveApplyModificator(s, genSettings, r).ApplyModificators()
        Next k
        Dim t1 As Integer = Environment.TickCount - t0
        '39000

    End Sub

    Public Shared Sub StackCreationRateTest()

        Dim target As RandStack = CreateRandStack()
        target.log.Disable()

        Dim t As Integer = Environment.TickCount

        Dim s As New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu5017", "g000uu5117", "g000uu5018", _
                                                                   "g000uu5018", "G000000000", "g000uu5018"}, _
                                                     New Integer() {1, 1, 5, 1, 0, 10}, Nothing, target), _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s, False)
        stats.LootCost = 2000
        stats.IGen = New AllDataStructues.LootGenSettings(False) With {.ConsumableItems = New AllDataStructues.ItemGenSettings With {.amount = 2}, _
                                                                       .NonconsumableItems = New AllDataStructues.ItemGenSettings With {.costPart = 0.333}, _
                                                                       .JewelItems = New AllDataStructues.ItemGenSettings With {.exclude = True, .costPart = 0.9}}

        Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)

        Dim stack As AllDataStructues.Stack
        For i As Integer = 1 To 2000 Step 1
            stack = target.Gen(gs)
        Next i
        Dim dt As Integer = Environment.TickCount - t
        '7660
        '5770
    End Sub
    Private Shared Function TestGenSettings(ByRef d As AllDataStructues.DesiredStats, Optional ByVal GroundTile As Boolean = True) As AllDataStructues.CommonStackCreationSettings
        Return New AllDataStructues.CommonStackCreationSettings _
            With {.StackStats = d, _
                  .deltaLeadership = 0, _
                  .groundTile = GroundTile, _
                  .noLeader = False, _
                  .pos = New Point(1, 1), _
                  .order = New AllDataStructues.Stack.StackOrder(AllDataStructues.Stack.StackOrder.OrderType.Stand, AllDataStructues.Stack.StackOrder.Settings.NoTarget, _
                                                                 AllDataStructues.Stack.StackOrder.OrderType.Normal, AllDataStructues.Stack.StackOrder.Settings.NoTarget)}
    End Function

    Public Shared Sub WaterCreation()
        Dim watergenerator As New WaterGen
        Dim comm As New Common(GenDefaultValues.DefaultMod)
        Dim settMap As New Map.SettingsMap With {.xSize = 52, _
                                                 .ySize = 52, _
                                                 .WaterAmount = 0.15, _
                                                 .nRaces = 1, _
                                                 .SymmetryClass = -1}
        Call settMap.Check()

        Dim grid As New Map(settMap.xSize - 1, settMap.ySize - 1, -1, comm)
        grid.Loc = {New Location(New Point(0, 0), 1, 1, 0, 1, False)}
        For y As Integer = 0 To grid.ySize Step 1
            For x As Integer = 0 To grid.xSize Step 1
                grid.board(x, y).locID = {1}
            Next x
        Next y
        grid.board(3, 3).mapObject.objectID = DefMapObjects.Types.Ruins
        grid.board(10, 4).mapObject.objectID = DefMapObjects.Types.Ruins
        grid.board(3, 15).mapObject.objectID = DefMapObjects.Types.Ruins
        grid.board(20, 40).mapObject.objectID = DefMapObjects.Types.Ruins

        grid.complited.StacksDesiredStatsGen_Done = True

        Dim genmesh As New ImpenetrableMeshGen(comm)
        genmesh.ObjectBlank = genmesh.ActiveObjectsSet(settMap, settMap.SymmetryClass)

        Dim t As Integer = Environment.TickCount
        Call watergenerator.Gen(grid, settMap, genmesh)
        Dim dt As Integer = Environment.TickCount - t
        '12153
        '10779
    End Sub

    Public Shared Sub StackRegeneration()
        Dim rStack As RandStack = CreateRandStack()
        Dim s As New AllDataStructues.Stack With {.items = New List(Of String)}
        s.units = AllDataStructues.Stack.UnitInfo.CreateArray(New String() {"g000uu5103", "g000uu5002", "g000uu0009", _
                                                                            "G000000000", "G000000000", "g000uu0007"}, _
                                                              New Integer() {1, 1, 2, 0, 0, 2}, _
                                                              Nothing, _
                                                              rStack)
        Dim stats As AllDataStructues.DesiredStats = rStack.StackStats(s, False)
        Dim nMelee, nRange, m, r As Integer
        For Each u As AllDataStructues.Stack.UnitInfo In s.units
            If Not u.unit.unitID = GenDefaultValues.emptyItem Then
                If u.unit.reach = GenDefaultValues.UnitAttackReach.melee Then
                    nMelee += 1
                Else
                    nRange += 1
                End If
            End If
        Next u
        Dim nomelee As Integer = 0
        Dim norange As Integer = 0
        Dim attempts As Integer = 100000
        For i As Integer = 1 To attempts Step 1
            Dim g = rStack.Gen(New AllDataStructues.CommonStackCreationSettings With {.groundTile = True, _
                                                                                      .StackStats = stats, _
                                                                                      .pos = New Point(1, 1)})
            m = 0
            r = 0
            For Each u As AllDataStructues.Stack.UnitInfo In g.units
                If Not u.unit.unitID = GenDefaultValues.emptyItem Then
                    If u.unit.reach = GenDefaultValues.UnitAttackReach.melee Then
                        m += 1
                    Else
                        r += 1
                    End If
                End If
            Next u
            If m = 0 Then nomelee += 1
            If r = 0 Then norange += 1
        Next i

        'old
        'nomelee  0
        'norange  1421

        'new
        'nomelee  0
        'norange  714
    End Sub

    Public Shared Sub LootRegenerator()
        Dim rStack As RandStack = CreateRandStack()
        Dim gm As New NevendaarTools.GameModel
        gm.Load(".\Resources", True)
        Dim AllItemsList() As AllDataStructues.Item = AllDataStructues.Item.getGameData(gm, rStack.comm)
        Dim attempts As Integer = 10000
        Dim items, result As New List(Of String)
        Dim settings As New AllDataStructues.CommonLootCreationSettings With {.pos = New Point(0, 0)}
        Dim nitems, m, resultGoldSum As Integer
        Dim totalGoldIn, totalGoldOut, totalItemsIn, totalItemsOut As Integer
        Dim minItemCostIn, maxItemCostIn, minItemCostOut, maxItemCostOut As Integer
        Dim minItemCostInT, maxItemCostInT, minItemCostOutT, maxItemCostOutT As Integer
        For i As Integer = 0 To attempts - 1 Step 1
            items.Clear()
            nitems = rStack.rndgen.RndInt(2, 20)
            minItemCostIn = Integer.MaxValue
            minItemCostOut = Integer.MaxValue
            maxItemCostIn = Integer.MinValue
            maxItemCostOut = Integer.MinValue
            For n As Integer = 1 To nitems Step 1
                m = -1
                Do While m = -1 OrElse rStack.LootCost(AllItemsList(m)).Gold = 0 OrElse AllItemsList(m).type = GenDefaultValues.ItemTypes.special
                    m = rStack.rndgen.RndInt(0, UBound(AllItemsList))
                Loop
                items.Add(AllItemsList(m).itemID)
                minItemCostIn = Math.Min(minItemCostIn, rStack.LootCost(AllItemsList(m)).Gold)
                maxItemCostIn = Math.Max(maxItemCostIn, rStack.LootCost(AllItemsList(m)).Gold)
            Next n
            settings.IGen = rStack.GetItemsGenSettings(items, False)
            settings.GoldCost = rStack.LootCost(items).Gold
            Dim goldIn As Integer = settings.GoldCost
            result = rStack.ItemsGen(settings)
            For Each r As String In result
                minItemCostOut = Math.Min(minItemCostOut, rStack.LootCost(r).Gold)
                maxItemCostOut = Math.Max(maxItemCostOut, rStack.LootCost(r).Gold)
            Next r
            resultGoldSum = AllDataStructues.Cost.Sum(rStack.LootCost(result))
            'Console.WriteLine(String.Join(vbTab, New String() {goldIn, resultGoldSum, _
            '                                      items.Count, result.Count, _
            '                                      Math.Round(goldIn / items.Count, 1), _
            '                                      Math.Round(resultGoldSum / result.Count, 1)}))
            totalGoldIn += goldIn
            totalGoldOut += resultGoldSum
            totalItemsIn += items.Count
            totalItemsOut += result.Count

            minItemCostInT += minItemCostIn
            minItemCostOutT += minItemCostOut
            maxItemCostInT += maxItemCostIn
            maxItemCostOutT += maxItemCostOut

            Call rStack.ResetAddedItems()
            Call rStack.ResetItemWeightMultiplier()
        Next i
        minItemCostInT /= attempts
        minItemCostOutT /= attempts
        maxItemCostInT /= attempts
        maxItemCostOutT /= attempts

        Console.WriteLine("--------------------")
        Console.WriteLine(String.Join(vbTab, New String() {totalGoldIn, totalGoldOut, _
                                              totalItemsIn, totalItemsOut, _
                                              Math.Round(totalGoldIn / totalItemsIn, 1), _
                                              Math.Round(totalGoldOut / totalItemsOut, 1), _
                                              "|", minItemCostInT, minItemCostOutT, maxItemCostInT, maxItemCostOutT}))
    End Sub

    Public Shared Sub SingleItemRegenerator()
        Dim rStack As RandStack = CreateRandStack()
        Dim gm As New NevendaarTools.GameModel
        gm.Load(".\Resources", True)
        Dim AllItemsList() As AllDataStructues.Item = AllDataStructues.Item.getGameData(gm, rStack.comm)
        Dim attempts As Integer = 10000
        Dim items, result As String
        Dim settings As New AllDataStructues.CommonLootCreationSettings With {.pos = New Point(0, 0)}
        Dim m, resultGoldSum As Integer
        Dim totalGoldIn, totalGoldOut As Integer

        Dim dx As Integer = 300
        Dim distribution(CInt(Math.Ceiling(10000 / dx)))() As Integer
        For i As Integer = 0 To UBound(distribution) Step 1
            distribution(i) = {0, 0}
        Next i
        For i As Integer = 0 To attempts - 1 Step 1
            m = -1
            Do While m = -1 OrElse rStack.LootCost(AllItemsList(m)).Gold = 0 OrElse AllItemsList(m).type = GenDefaultValues.ItemTypes.special
                m = rStack.rndgen.RndInt(0, UBound(AllItemsList))
            Loop
            items = AllItemsList(m).itemID
            settings.IGen = rStack.GetItemsGenSettings({items}.ToList, True)
            settings.GoldCost = rStack.LootCost(items).Gold
            Dim goldIn As Integer = settings.GoldCost

            result = rStack.ThingGen(settings)
            resultGoldSum = AllDataStructues.Cost.Sum(rStack.LootCost(result))

            'Console.WriteLine(String.Join(vbTab, New String() {goldIn, resultGoldSum}))

            totalGoldIn += goldIn
            totalGoldOut += resultGoldSum

            distribution(Math.Floor(goldIn / dx))(0) += 1
            distribution(Math.Floor(resultGoldSum / dx))(1) += 1

            Call rStack.ResetAddedItems()
            Call rStack.ResetItemWeightMultiplier()
        Next i
        Console.WriteLine("--------------------")
        Console.WriteLine(String.Join(vbTab, New String() {totalGoldIn, totalGoldOut, _
                                              Math.Round(totalGoldIn / totalGoldOut, 3)}))
        Dim d As String = "cost" & vbTab & "in" & vbTab & "out"
        For i As Integer = 0 To UBound(distribution) Step 1
            d &= vbNewLine & i * dx + dx / 2 & vbTab & distribution(i)(0) & vbTab & distribution(i)(1)
        Next i

    End Sub

End Class
