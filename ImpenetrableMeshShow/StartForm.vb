Imports RandomStackGenerator

Friend Class StartForm

    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector
    Dim comm As New Common(GenDefaultValues.DefaultMod)
    Dim ObjectsSize As New Dictionary(Of String, Size)

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

        'Call Tests.ItemsGenTest_time()

        If GenDefaultValues.writeToConsole Then
            Dim tf As New TemplateForge(RandomStackGenerator.GenDefaultValues.TextLanguage.Rus)
            tf.ReadCommonMapSettingsFromFile(".\Resources", "example_template_2_unsymm.txt")

            For Each lang As GenDefaultValues.TextLanguage In System.Enum.GetValues(GetType(GenDefaultValues.TextLanguage))
                Call TemplateForge.GetPermissibleParametersRange(lang)
            Next lang
            Dim param() As TemplateForge.Parameter = TemplateForge.GetPermissibleParametersRange(GenDefaultValues.TextLanguage.Rus)
        End If

        Dim treesAmount() As Integer = {0, 20, 20, 20, 20, 20, 0, 0, 0, 0, 0, 0, 0, 0, 20}

        Dim rstack As New RandStack(TestDataRead.DefaultGenData)
        Dim objCont As New ObjectsContentSet(rstack)

        'Dim ds As New AllDataStructues.DesiredStats With {.ExpBarAverage = 50, .ExpStackKilled = 56, .MaxGiants = 3, _
        '                                                  .MeleeCount = 3, .Race = New List(Of Integer), .StackSize = 6}
        'ds.Race.Add(16)
        'Dim dds As AllDataStructues.DesiredStats = AllDataStructues.DesiredStats.Copy(ds)
        'Call rstack.GenFingters(ds, dds, 3, -1, True, 6, True, New List(Of Integer), 0, 0.5, -1)

        comm.excludedObjects = rstack.comm.excludedObjects
        Dim objSizeArray() As ImpenetrableObjects.GlobalMapDecoration = ReadObjSize()

        Dim objplace As New ImpenetrableObjects(objSizeArray, False, TestDataRead.ReadTestSpells, comm)

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

        Dim startTime As Integer = Environment.TickCount
        grid = New MapGenWrapper(objplace).CommonGen(gsettings, genTimeLimit, GenDefaultValues.DefaultMod)

        If Not IsNothing(grid.board) Then
            Call ShowResult(grid)
            Call shortMapFormat.MapConversion(grid, gsettings, objSizeArray, objCont, True, True, treesAmount, GenDefaultValues.TextLanguage.Rus)
        End If
        Dim endTime As Integer = Environment.TickCount
        Console.WriteLine(endTime - startTime & " ms")

        If GenDefaultValues.writeToConsole Then Console.WriteLine(grid.log.PrintAll)
        If GenDefaultValues.writeToConsole Then LogTextBox.Text = grid.log.PrintAll
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

    Private Function ReadObjSize() As ImpenetrableObjects.GlobalMapDecoration()
        Dim t() As String = ValueConverter.TxtSplit(My.Resources.TestObjectSize)
        Dim result(UBound(t) - 1) As ImpenetrableObjects.GlobalMapDecoration
        ObjectsSize.Clear()
        For i As Integer = 1 To UBound(t) Step 1
            Dim r() As String = t(i).Split(CChar(" "))
            ObjectsSize.Add(r(0).ToUpper, New Size(CInt(r(1)), CInt(r(2))))
            result(i - 1) = New ImpenetrableObjects.GlobalMapDecoration With {.ID = r(0).ToUpper, .Size = New Size(CInt(r(1)), CInt(r(2)))}
        Next i
        Return result
    End Function

End Class

Public Class TestDataRead

    Public Shared Function ReadTestSpells() As AllDataStructues.Spell()
        Dim spells()() As String = TXTSplit(My.Resources.TestSpells, 5)
        Dim rspells()() As String = TXTSplit(My.Resources.TestSpellsRace, 3)

        Dim spellsHeader As Dictionary(Of String, Integer) = MakeHeader(spells(0))

        Dim res(UBound(spells) - 1) As AllDataStructues.Spell
        For i As Integer = 1 To UBound(spells) Step 1
            Dim s() As String = spells(i)
            res(i - 1) = New AllDataStructues.Spell With { _
                    .area = GetField(spellsHeader, "AREA", s), _
                    .castCost = AllDataStructues.Cost.Read(GetField(spellsHeader, "CASTING_C", s)), _
                    .category = GetField(spellsHeader, "CATEGORY", s), _
                    .level = GetField(spellsHeader, "LEVEL", s), _
                    .spellID = GetField(spellsHeader, "SPELL_ID", s).ToUpper, _
                    .name = GetField(spellsHeader, "SPELL_ID", s) & "_test"}
        Next i
        For i As Integer = 1 To UBound(rspells) Step 1
            Dim s() As String = rspells(i)
            For j As Integer = 0 To UBound(res) Step 1
                If res(j).spellID.ToUpper = s(1).ToUpper Then
                    res(j).researchCost.Add(s(0).ToUpper, AllDataStructues.Cost.Read(s(2)))
                    Exit For
                End If
            Next j
        Next i
        Return res
    End Function

    Public Shared Function ReadTestUnits(ByVal modName As String) As AllDataStructues.Unit()
        Dim units()() As String = TXTSplit(My.Resources.TestUnitsTable, 38)
        Dim attacks()() As String = TXTSplit(My.Resources.TestAttacksTable, 19)
        Dim gimmu()() As String = TXTSplit(My.Resources.TestGImmuTable, 3)
        Dim gimmuc()() As String = TXTSplit(My.Resources.TestGImmuCTable, 3)
        Dim dynupgr()() As String = TXTSplit(My.Resources.TestDynUpgr, 16)

        Dim unitsHeader As Dictionary(Of String, Integer) = MakeHeader(units(0))
        Dim attacksHeader As Dictionary(Of String, Integer) = MakeHeader(attacks(0))
        Dim gimmuHeader As Dictionary(Of String, Integer) = MakeHeader(gimmu(0))
        Dim gimmucHeader As Dictionary(Of String, Integer) = MakeHeader(gimmuc(0))
        Dim dynupgrHeader As Dictionary(Of String, Integer) = MakeHeader(dynupgr(0))

        Dim UnitsList(UBound(units) - 1) As AllDataStructues.Unit
        For i As Integer = 1 To UBound(units) Step 1
            Dim unit() As String = units(i)
            UnitsList(i - 1) = New AllDataStructues.Unit
            UnitsList(i - 1).unitID = GetField(unitsHeader, "UNIT_ID", unit)
            UnitsList(i - 1).name = UnitsList(i - 1).unitID & "_test"
            UnitsList(i - 1).level = GetField(unitsHeader, "LEVEL", unit)
            UnitsList(i - 1).race = GetField(unitsHeader, "SUBRACE", unit)
            UnitsList(i - 1).unitBranch = GetField(unitsHeader, "BRANCH", unit)
            UnitsList(i - 1).small = GetField(unitsHeader, "SIZE_SMALL", unit)
            UnitsList(i - 1).EXPkilled = GetField(unitsHeader, "XP_KILLED", unit)
            UnitsList(i - 1).EXPnext = GetField(unitsHeader, "XP_NEXT", unit)
            UnitsList(i - 1).dynUpgradeLevel = GetField(unitsHeader, "DYN_UPG_LV", unit)
            UnitsList(i - 1).leadership = GetField(unitsHeader, "LEADERSHIP", unit)
            UnitsList(i - 1).waterOnly = GetField(unitsHeader, "WATER_ONLY", unit)
            UnitsList(i - 1).unitCost = AllDataStructues.Cost.Read(GetField(unitsHeader, "ENROLL_C", unit))
            UnitsList(i - 1).hp = GetField(unitsHeader, "HIT_POINT", unit)
            UnitsList(i - 1).armor = GetField(unitsHeader, "ARMOR", unit)

            Dim attackID As String = GetField(unitsHeader, "ATTACK_ID", unit)
            For j As Integer = 1 To UBound(attacks) Step 1
                Dim attack() As String = attacks(j)
                If attackID.ToUpper = GetField(attacksHeader, "ATT_ID", attack).ToUpper Then
                    UnitsList(i - 1).reach = GetField(attacksHeader, "REACH", attack)
                    UnitsList(i - 1).initiative = GetField(attacksHeader, "INITIATIVE", attack)
                    UnitsList(i - 1).accuracy = GetField(attacksHeader, "POWER", attack)
                    UnitsList(i - 1).damage = GetField(attacksHeader, "QTY_DAM", attack)
                    UnitsList(i - 1).heal = GetField(attacksHeader, "QTY_HEAL", attack)
                    If UnitsList(i - 1).initiative = 0 Then
                        i = i
                    End If
                    Exit For
                End If
                If j = UBound(attacks) Then Throw New Exception
            Next j
            UnitsList(i - 1).ASourceImmunity = ReadImmunity(UnitsList(i - 1).unitID, gimmuHeader, gimmu)
            UnitsList(i - 1).AClassImmunity = ReadImmunity(UnitsList(i - 1).unitID, gimmucHeader, gimmuc)
            UnitsList(i - 1).dynUpgrade1 = ReadDynUpgr(GetField(unitsHeader, "DYN_UPG1", unit), dynupgrHeader, dynupgr)
            UnitsList(i - 1).dynUpgrade2 = ReadDynUpgr(GetField(unitsHeader, "DYN_UPG2", unit), dynupgrHeader, dynupgr)
        Next i
        Return UnitsList
    End Function
    Private Shared Function ReadImmunity(ByRef unitID As String, _
                                         ByRef header As Dictionary(Of String, Integer), _
                                         ByRef data()() As String) As Dictionary(Of Integer, Integer)
        Dim r As New Dictionary(Of Integer, Integer)
        For j As Integer = 1 To UBound(data) Step 1
            Dim immu() As String = data(j)
            If unitID.ToUpper = GetField(header, "UNIT_ID", immu).ToUpper Then
                r.Add(GetField(header, "IMMUNITY", immu), _
                               GetField(header, "IMMUNECAT", immu))
            End If
        Next j
        Return r
    End Function
    Private Shared Function ReadDynUpgr(ByRef dynUpgrID As String, _
                                        ByRef header As Dictionary(Of String, Integer), _
                                        ByRef data()() As String) As AllDataStructues.DynUpgrade
        Dim r As New AllDataStructues.DynUpgrade
        For j As Integer = 1 To UBound(data) Step 1
            Dim upgr() As String = data(j)
            If dynUpgrID.ToUpper = GetField(header, "UPGRADE_ID", upgr).ToUpper Then
                r.accuracy = GetField(header, "POWER", upgr)
                r.armor = GetField(header, "ARMOR", upgr)
                r.damage = GetField(header, "DAMAGE", upgr)
                r.EXPkilled = GetField(header, "XP_KILLED", upgr)
                r.EXPnext = GetField(header, "XP_NEXT", upgr)
                r.heal = GetField(header, "HEAL", upgr)
                r.hp = GetField(header, "HIT_POINT", upgr)
                r.initiative = GetField(header, "INITIATIVE", upgr)
                r.unitCost = AllDataStructues.Cost.Read(GetField(header, "ENROLL_C", upgr))
                Exit For
            End If
            If j = UBound(data) Then Throw New Exception
        Next j
        Return r
    End Function

    Public Shared Function ReadTestItems(ByVal modName As String) As AllDataStructues.Item()
        Dim items()() As String = TXTSplit(My.Resources.TestItemsTable, 3)

        Dim itemsHeader As Dictionary(Of String, Integer) = MakeHeader(items(0))

        Dim ItemsList(UBound(items) - 1) As AllDataStructues.Item
        For i As Integer = 1 To UBound(items) Step 1
            Dim r() As String = items(i)
            ItemsList(i - 1) = New AllDataStructues.Item
            ItemsList(i - 1).type = GetField(itemsHeader, "ITEM_CAT", r)
            ItemsList(i - 1).itemID = GetField(itemsHeader, "ITEM_ID", r)
            ItemsList(i - 1).name = ItemsList(i - 1).itemID & "_test"
            ItemsList(i - 1).itemCost = AllDataStructues.Cost.Read(GetField(itemsHeader, "VALUE", r))
        Next i
        Return ItemsList
    End Function

    Public Shared Function ReadTestModificators() As AllDataStructues.Modificator()
        Dim gmodif()() As String = TXTSplit(My.Resources.TestGModif, 2)
        Dim gmodifL()() As String = TXTSplit(My.Resources.TestGModifL, 11)

        Dim gmodifHeader As Dictionary(Of String, Integer) = MakeHeader(gmodif(0))
        Dim gmodiflHeader As Dictionary(Of String, Integer) = MakeHeader(gmodifL(0))

        Dim result(UBound(gmodif) - 1) As AllDataStructues.Modificator
        For i As Integer = 1 To UBound(gmodif) Step 1
            result(i - 1) = New AllDataStructues.Modificator With { _
                                .id = GetField(gmodifHeader, "MODIF_ID", gmodif(i)), _
                                .source = GetField(gmodifHeader, "SOURCE", gmodif(i))}
            For j As Integer = 1 To UBound(gmodifL) Step 1
                Dim s() As String = gmodifL(j)
                If result(i - 1).id.ToUpper = GetField(gmodiflHeader, "BELONGS_TO", s).ToUpper Then
                    result(i - 1).effect.Add(New AllDataStructues.Modificator.ModifEffect With { _
                                                .type = GetField(gmodiflHeader, "TYPE", s), _
                                                .percent = GetField(gmodiflHeader, "PERCENT", s), _
                                                .number = GetField(gmodiflHeader, "NUMBER", s), _
                                                .ability = GetField(gmodiflHeader, "ABILITY", s), _
                                                .immunASource = GetField(gmodiflHeader, "IMMUNITY", s), _
                                                .immunASourceCat = GetField(gmodiflHeader, "IMMUNECAT", s), _
                                                .move = GetField(gmodiflHeader, "MOVE", s), _
                                                .immunAClass = GetField(gmodiflHeader, "IMMUNITYC", s), _
                                                .immunAClassCat = GetField(gmodiflHeader, "IMMUNECATC", s)})
                End If
            Next j
            If result(i - 1).effect.Count = 0 Then Throw New Exception
        Next i
        Return result
    End Function

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
    Private Shared Function MakeHeader(ByRef header() As String) As Dictionary(Of String, Integer)
        Dim r As New Dictionary(Of String, Integer)
        For i As Integer = 0 To UBound(header) Step 1
            r.Add(header(i), i)
        Next i
        Return r
    End Function
    Private Shared Function GetField(ByRef header As Dictionary(Of String, Integer), _
                                     ByRef name As String, _
                                     ByRef data() As String) As String
        If header.ContainsKey(name) Then
            Return Data(header.Item(name))
        Else
            Throw New Exception("Unexpected field name: " & name)
        End If
    End Function

    Public Shared Function DefaultGenData() As RandStack.ConstructorInput
        Dim r As New RandStack.ConstructorInput
        r.AllUnitsList = ReadTestUnits(GenDefaultValues.DefaultMod)
        r.AllItemsList = ReadTestItems(GenDefaultValues.DefaultMod)
        r.AllSpellsList = ReadTestSpells()
        r.AllModificatorsList = ReadTestModificators()
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

        ' lootTime = ItemsGenTest() '12761
        ' mercenariesTime = MakeMercenariesListTest() '18073
        ' merchantTime = MakeMerchItemsListTest() '13701
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
        Dim rstackData As New RandStack.ConstructorInput With {.AllUnitsList = TestDataRead.ReadTestUnits(GenDefaultValues.DefaultMod), _
                                                               .AllItemsList = TestDataRead.ReadTestItems(GenDefaultValues.DefaultMod), _
                                                               .AllSpellsList = TestDataRead.ReadTestSpells(), _
                                                               .AllModificatorsList = TestDataRead.ReadTestModificators()}
        rstackData.settings.modName = GenDefaultValues.DefaultMod
        rstackData.mapData.capitalPos = {New Point(10, 10)}
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
                    target.settings.ApplyStrictTypesFilter = (strictFilter = 1)
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
End Class
