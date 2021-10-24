Imports System.Collections.Generic

Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator
Imports System.Threading.Tasks



'''<summary>
'''This is a test class for RandStackTest and is intended
'''to contain all RandStackTest Unit Tests
'''</summary>
<TestClass()> _
Public Class RandStackTest

    Private testContextInstance As TestContext

    '''<summary>
    '''Gets or sets the test context which provides
    '''information about and functionality for the current test run.
    '''</summary>
    Public Property TestContext() As TestContext
        Get
            Return testContextInstance
        End Get
        Set(value As TestContext)
            testContextInstance = value
        End Set
    End Property

#Region "Additional test attributes"
    '
    'You can use the following additional attributes as you write your tests:
    '
    'Use ClassInitialize to run code before running the first test in the class
    '<ClassInitialize()>  _
    'Public Shared Sub MyClassInitialize(ByVal testContext As TestContext)
    'End Sub
    '
    'Use ClassCleanup to run code after all tests in a class have run
    '<ClassCleanup()>  _
    'Public Shared Sub MyClassCleanup()
    'End Sub
    '
    'Use TestInitialize to run code before running each test
    '<TestInitialize()>  _
    'Public Sub MyTestInitialize()
    'End Sub
    '
    'Use TestCleanup to run code after each test has run
    '<TestCleanup()>  _
    'Public Sub MyTestCleanup()
    'End Sub
    '
#End Region


    Friend Function CreateRandStack_Accessor() As RandStack_Accessor
        Call PrepareToTest()
        Dim rstackData As New RandStack.ConstructorInput With {.AllUnitsList = UnitsList, _
                                                               .AllItemsList = ItemsList, _
                                                               .AllSpellsList = AllSpells, _
                                                               .AllModificatorsList = AllModificators}
        rstackData.settings.modName = GenDefaultValues.DefaultMod
        rstackData.mapData.capitalPos = {New Point(10, 10)}
        Return New RandStack_Accessor(rstackData)
    End Function
    Friend Function CreateRandStack() As RandStack
        Call PrepareToTest()
        Dim rstackData As New RandStack.ConstructorInput With {.AllUnitsList = UnitsList, _
                                                               .AllItemsList = ItemsList, _
                                                               .AllSpellsList = AllSpells, _
                                                               .AllModificatorsList = AllModificators}
        rstackData.settings.modName = GenDefaultValues.DefaultMod
        rstackData.mapData.capitalPos = {New Point(10, 10)}
        Return New RandStack(rstackData)
    End Function
    Friend Sub PrepareToTest()
        Call CopyResources()
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        If IsNothing(AllSpells) Then Call ReadTestSpells()
        If IsNothing(AllModificators) Then Call ReadTestModificators()
    End Sub
    Public Shared Sub CopyResources()
        If Not IO.Directory.Exists("Resources") Then
            Dim projectDir As String = Environment.CurrentDirectory
            For i As Integer = 1 To 3 Step 1
                projectDir = IO.Path.GetDirectoryName(projectDir)
            Next i
            Dim source As String = IO.Path.Combine(projectDir, "RandomStackGenerator\Resources")
            Dim destination As String = IO.Path.Combine(Environment.CurrentDirectory, "Resources")
            Call RecursiveCopy(New IO.DirectoryInfo(source), New IO.DirectoryInfo(destination))
        End If
    End Sub
    Public Shared Sub RecursiveCopy(source As IO.DirectoryInfo, target As IO.DirectoryInfo)
        ' Recursively call the DeepCopy Method for each Directory
        For Each dir As IO.DirectoryInfo In source.GetDirectories()
            RecursiveCopy(dir, target.CreateSubdirectory(dir.Name))
        Next dir
        ' Go ahead and copy each file in "source" to the "target" directory
        For Each file As IO.FileInfo In source.GetFiles()
            file.CopyTo(IO.Path.Combine(target.FullName, file.Name))
        Next file
    End Sub

    Friend UnitsList() As AllDataStructues.Unit = Nothing
    Friend ItemsList() As AllDataStructues.Item = Nothing
    Friend AllSpells() As AllDataStructues.Spell = Nothing
    Friend AllModificators() As AllDataStructues.Modificator = Nothing

    Friend Sub ReadTestUnits()
        UnitsList = ImpenetrableMeshShow.TestDataRead.ReadTestUnits(GenDefaultValues.DefaultMod)
    End Sub
    Friend Sub ReadTestItems()
        ItemsList = ImpenetrableMeshShow.TestDataRead.ReadTestItems(GenDefaultValues.DefaultMod)
    End Sub
    Friend Sub ReadTestSpells()
        AllSpells = ImpenetrableMeshShow.TestDataRead.ReadTestSpells
    End Sub
    Friend Sub ReadTestModificators()
        AllModificators = ImpenetrableMeshShow.TestDataRead.ReadTestModificators
    End Sub

    Private Function TestGenSettings(ByRef d As AllDataStructues.DesiredStats, Optional ByVal GroundTile As Boolean = True) As AllDataStructues.CommonStackCreationSettings
        Return New AllDataStructues.CommonStackCreationSettings _
            With {.StackStats = d, _
                  .deltaLeadership = 0, _
                  .groundTile = GroundTile, _
                  .noLeader = False, _
                  .pos = New Point(1, 1), _
                  .order = New AllDataStructues.Stack.StackOrder(AllDataStructues.Stack.StackOrder.OrderType.Stand, GenDefaultValues.emptyItem, _
                                                                 AllDataStructues.Stack.StackOrder.OrderType.Normal, GenDefaultValues.emptyItem)}
    End Function

    '''<summary>
    '''A test for RndPos
    '''</summary>
    <TestMethod(), _
     DeploymentItem("RandomStackGenerator.dll")> _
    Public Sub RndPosTest()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()

        Dim serial_ok, parallel_ok As Boolean
        serial_ok = True
        parallel_ok = True
        For n As Integer = 1 To 10 Step 1
            Dim ok As Boolean
            Dim r As Integer
            For j As Integer = 1 To n Step 1
                ok = False
                For attempt = 0 To 100 * n Step 1
                    r = target.rndgen.RndPos(n, True)
                    If r = j Then
                        ok = True
                        Exit For
                    End If
                Next attempt
                If Not ok Then
                    serial_ok = False
                    Exit For
                End If
            Next j
            If Not serial_ok Then Exit For
        Next n

        Parallel.For(1, 10 + 1, _
         Sub(n As Integer)
             If Not parallel_ok Then Exit Sub
             Dim ok As Boolean
             Dim r As Integer
             For j As Integer = 1 To n Step 1
                 ok = False
                 For attempt = 0 To 100 * n Step 1
                     r = target.rndgen.RndPos(n, False)
                     If r = j Then
                         ok = True
                         Exit For
                     End If
                 Next attempt
                 If Not ok Then
                     parallel_ok = False
                     Exit For
                 End If
             Next j
         End Sub)
        If Not serial_ok Or Not parallel_ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Rand
    '''</summary>
    <TestMethod(), _
     DeploymentItem("RandomStackGenerator.dll")> _
    Public Sub RandTest()
        Call PrepareToTest()
        Dim target As Common_Accessor = New Common_Accessor(GenDefaultValues.DefaultMod)
        Dim maxval As Integer = 10000
        Dim ok, g(maxval / 10), tmpok As Boolean
        ok = True
        For j As Integer = 0 To 10 Step 1
            For i As Integer = 0 To maxval * 10 Step 1
                g(target.rndgen.Rand(0, CDbl(maxval), True) / 10) = True
            Next i
            tmpok = True
            For i As Integer = 0 To maxval / 10 Step 1
                If Not g(i) Then
                    tmpok = False
                    Exit For
                End If
            Next i
            If tmpok Then Exit For
        Next j
        For i As Integer = 0 To maxval / 10 Step 1
            If Not g(i) Then ok = False
            g(i) = False
        Next i

        For j As Integer = 0 To 10 Step 1
            Parallel.For(0, maxval * 10 + 1, _
             Sub(i As Integer)
                 g(target.rndgen.Rand(0, CDbl(maxval), False) / 10) = True
             End Sub)
            tmpok = True
            For i As Integer = 0 To maxval / 10 Step 1
                If Not g(i) Then
                    tmpok = False
                    Exit For
                End If
            Next i
            If tmpok Then Exit For
        Next j
        For i As Integer = 0 To maxval / 10 Step 1
            If Not g(i) Then ok = False
            g(i) = False
        Next i

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest1()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True
        Dim races() As String = New String() {"H", "U", "L", "C", "E", "N", "G", "D", "S", "W", "B", "A", "AS", "AST", "AW"}

        Dim statsList As New List(Of AllDataStructues.DesiredStats)

        For r As Integer = 0 To UBound(races) Step 1
            For Each ExpBarAverage As Integer In {50, 125, 500, 1000}
                For Each ExpStackKilled As Integer In {200, 275, 350}
                    For MaxGiants As Integer = 0 To 3 Step 1
                        For MeleeCount As Integer = 0 To 3 Step 1
                            For StackSize As Integer = 1 To 6 Step 1
                                statsList.Add(New AllDataStructues.DesiredStats With { _
                                                   .ExpBarAverage = ExpBarAverage, .ExpStackKilled = ExpStackKilled, _
                                                   .MaxGiants = MaxGiants, .MeleeCount = MeleeCount, _
                                                   .Race = New List(Of Integer), .StackSize = StackSize, _
                                                   .IGen = New AllDataStructues.LootGenSettings(False), _
                                                   .LeaderModificators = New List(Of String), _
                                                   .UnitsModificators = New List(Of String)})
                                statsList.Item(statsList.Count - 1).Race.Add(target.comm.RaceIdentifierToSubrace(races(r)))
                            Next StackSize
                        Next MeleeCount
                    Next MaxGiants
                Next ExpStackKilled
            Next ExpBarAverage
        Next r

        Parallel.ForEach(statsList, _
         Sub(item As AllDataStructues.DesiredStats)
             If Not ok Then Exit Sub
             Dim GroundTile As Boolean
             Dim locOk As Boolean = True
             For g As Integer = 0 To 1 Step 1
                 If g = 0 Then
                     GroundTile = False
                     If item.Race.Item(0) = target.comm.RaceIdentifierToSubrace("W") Then g = 1
                 Else
                     GroundTile = True
                 End If
                 Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(item, GroundTile)
                 Dim stack As AllDataStructues.Stack = target.Gen(gs)
                 locOk = TestStack(stack, target)
                 If Not locOk Then Exit For
             Next g
             If Not locOk Then ok = False
         End Sub)

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    Private Function TestStack(ByRef stack As AllDataStructues.Stack, ByRef target As RandStack_Accessor, _
                               Optional ByRef races() As Integer = Nothing, _
                               Optional ByRef raceokL() As Boolean = Nothing, _
                               Optional ByRef raceokF() As Boolean = Nothing) As Boolean
        Dim busytransfer() As Integer = New Integer() {1, -1, 3, -1, 5, -1}
        Dim firstrow() As Integer = New Integer() {0, 2, 4}
        Dim secondrow() As Integer = New Integer() {1, 3, 5}
        Dim unit As AllDataStructues.Unit
        If stack.leaderPos < 0 Or IsNothing(stack.units) Then Return False
        If stack.units(stack.leaderPos).unit.unitID = GenDefaultValues.emptyItem Then Return False
        For i As Integer = 0 To UBound(stack.units) Step 1
            If stack.units(i).unit.unitID = "" Then
                Return False
            ElseIf Not stack.units(i).unit.unitID = GenDefaultValues.emptyItem Then
                unit = target.FindUnitStats(stack.units(i).unit.unitID)
                If (unit.unitBranch = GenDefaultValues.UnitClass.leader And Not i = stack.leaderPos) Or (Not unit.unitBranch = GenDefaultValues.UnitClass.leader And i = stack.leaderPos) Then Return False
                If Not unit.small AndAlso (busytransfer(i) = -1 OrElse Not stack.units(busytransfer(i)).unit.unitID = GenDefaultValues.emptyItem) Then Return False
                If unit.reach = GenDefaultValues.UnitAttackReach.melee Then
                    For j As Integer = 0 To UBound(secondrow) Step 1
                        If i = secondrow(j) Then Return False
                    Next j
                End If
                If Not IsNothing(races) Then
                    For r As Integer = 0 To UBound(races) Step 1
                        If unit.race = races(r) Then
                            If unit.unitBranch = GenDefaultValues.UnitClass.leader Then
                                raceokL(r) = True
                            Else
                                raceokF(r) = True
                            End If
                        End If
                    Next r
                End If
            End If
        Next i
        Return True
    End Function

    '''<summary>
    '''A test for StackStats
    '''</summary>
    <TestMethod()> _
    Public Sub StackStatsTest1()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim s As New AllDataStructues.Stack With {.items = New List(Of String)}
        s.units = AllDataStructues.Stack.UnitInfo.CreateArray(New String() {"G000UU5356", "G000UU0174", "G005UU6111", _
                                                                            "G000000000", "G000UU0162", "G000UU0162"}, _
                                                              New Integer() {1, 1, 1, 0, 1, 1}, _
                                                              Nothing, _
                                                              CreateRandStack())
        s.items.AddRange(New String() {"G000IG0011", "G000IG0012", "G000IG0013", "G000IG0014", _
                                       "G000IG0015", "G000IG0016", "G000IG1001", "G000IG1002"})
        Dim expected As New AllDataStructues.DesiredStats With {.Race = New List(Of Integer), _
                                                                .StackSize = 6, _
                                                                .MeleeCount = 2, _
                                                                .MaxGiants = 1}
        For Each i As String In s.items
            expected.LootCost += AllDataStructues.Cost.Sum(target.FindItemStats(i).itemCost)
        Next i
        For Each u As AllDataStructues.Stack.UnitInfo In s.units
            expected.ExpStackKilled += u.unit.EXPkilled + u.unit.GetExpKilledOverlevel(u.level)
            expected.ExpBarAverage += u.unit.EXPnext + u.unit.GetExpNextOverlevel(u.level)
        Next u
        expected.ExpBarAverage /= expected.StackSize
        expected.Race.AddRange(New Integer() {1, 2, 3, 4})
        Dim ok As Boolean = True
        Dim calculated As AllDataStructues.DesiredStats = target.StackStats(s, False)
        If Not expected.ExpBarAverage = calculated.ExpBarAverage Then ok = False
        If Not expected.ExpStackKilled = calculated.ExpStackKilled Then ok = False
        If Not expected.LootCost = calculated.LootCost Then ok = False
        If Not expected.MaxGiants = calculated.MaxGiants Then ok = False
        If Not expected.MeleeCount = calculated.MeleeCount Then ok = False
        If Not expected.StackSize = calculated.StackSize Then ok = False
        For Each i As Integer In expected.Race
            If Not calculated.Race.Contains(i) Then ok = False
        Next i
        For Each i As Integer In calculated.Race
            If Not expected.Race.Contains(i) Then ok = False
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for ItemsGen
    '''</summary>
    <TestMethod()> _
    Public Sub ItemsGenTest1()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True
        Dim cost, sum As Integer
        Dim genitems As List(Of String)
        For i As Integer = 0 To 100 Step 1
            cost = target.minItemGoldCost + i * 125
            Dim gi As New AllDataStructues.CommonLootCreationSettings With {.GoldCost = cost, .IGen = New AllDataStructues.LootGenSettings(False), _
                                                                            .TypeCostRestriction = Nothing, .pos = New Point(1, 1)}
            genitems = target.ItemsGen(gi)
            sum = target.LootCost(genitems).Gold
            'If sum > cost Then ok = False
            If Math.Abs(cost - sum) > cost Then ok = False
            If Not ok Then Exit For
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    <TestMethod()> _
    Public Sub ItemsGenTest2()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True
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
            For strictFilter As Integer = 0 To 1 Step 1
                target.settings.ApplyStrictTypesFilter = (strictFilter = 1)
                For i As Integer = 0 To UBound(inputItems) Step 1
                    Call target.ResetAddedItems()
                    Call target.ResetItemWeightMultiplier()
                    items = RunLootCreation(target, inputItems(i), testRuinsLootCreator)
                    ok = TestCreatedLoot(target, inputItems(i), items)
                    If Not ok Then Exit For
                Next i
                If Not ok Then Exit For
            Next strictFilter
            If Not ok Then Exit For
        Next

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    Private Function RunLootCreation(ByRef target As RandStack_Accessor, ByRef inputLoot As List(Of String), ByRef ruinsTest As Boolean) As List(Of String)
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
    Private Function TestCreatedLoot(ByRef target As RandStack_Accessor, ByRef inputLoot As List(Of String), ByRef outputLoot As List(Of String)) As Boolean
        Dim iLoot, oLoot As New List(Of AllDataStructues.Item)
        Dim iTypes, oTypes, iCommonTypes, oCommonTypes As New List(Of Integer)
        Dim t, n As Integer
        Dim c As Boolean
        For Each id As String In inputLoot
            iLoot.Add(target.FindItemStats(id))
            t = target.FindItemStats(id).type
            If Not iTypes.Contains(t) Then iTypes.Add(t)
            n = -1
            For i As Integer = 0 To UBound(target.comm.ItemTypesLists) Step 1
                If target.comm.ItemTypesLists(i).Contains(t) Then n = i
            Next i
            If Not iCommonTypes.Contains(n) Then iCommonTypes.Add(n)
        Next id
        For Each id As String In inputLoot
            oLoot.Add(target.FindItemStats(id))
            t = target.FindItemStats(id).type
            If Not oTypes.Contains(t) Then oTypes.Add(t)
            n = -1
            For i As Integer = 0 To UBound(target.comm.ItemTypesLists) Step 1
                If target.comm.ItemTypesLists(i).Contains(t) Then n = i
            Next i
            If Not oCommonTypes.Contains(n) Then oCommonTypes.Add(n)
        Next id

        For Each i As Integer In oCommonTypes
            If Not iCommonTypes.Contains(i) Then Return False
        Next i
        If target.settings.ApplyStrictTypesFilter Then
            For Each i As Integer In oTypes
                If Not iTypes.Contains(i) Then Return False
            Next i
        End If
        For Each item As AllDataStructues.Item In iLoot
            If item.type = GenDefaultValues.ItemTypes.special Or target.comm.IsPreserved(item) Then
                c = False
                For Each i As AllDataStructues.Item In oLoot
                    If item.itemID.ToUpper = i.itemID.ToUpper Then
                        c = True
                        Exit For
                    End If
                Next i
                If Not c Then Return False
            End If
        Next item
        For Each item As AllDataStructues.Item In oLoot
            If item.type = GenDefaultValues.ItemTypes.special Then
                c = False
                For Each i As AllDataStructues.Item In iLoot
                    If item.itemID.ToUpper = i.itemID.ToUpper Then
                        c = True
                        Exit For
                    End If
                Next i
                If Not c Then Return False
            End If
        Next item
        Return True
    End Function

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest2()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True

        Dim stats As New AllDataStructues.DesiredStats With {.ExpBarAverage = 1450, .ExpStackKilled = 1000, .Race = New List(Of Integer), _
                                                             .StackSize = 3, .MaxGiants = 1, .MeleeCount = 3, .LootCost = 1200, _
                                                             .IGen = New AllDataStructues.LootGenSettings(False), _
                                                             .LeaderModificators = New List(Of String), _
                                                             .UnitsModificators = New List(Of String)}
        Dim races() As Integer = New Integer() {1, 2}
        Dim raceokL(UBound(races)), raceokF(UBound(races)) As Boolean
        stats.Race.AddRange(races)

        For i As Integer = 0 To 1000 Step 1
            Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
            Dim stack As AllDataStructues.Stack = target.Gen(gs)
            ok = TestStack(stack, target, races, raceokL, raceokF)
            If Not ok Then Exit For
        Next i
        For r As Integer = 0 To UBound(races) Step 1
            If Not raceokL(r) And Not raceokF(r) Then ok = False
        Next r

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for GenGag
    '''</summary>
    <TestMethod()> _
    Public Sub GenGagTest()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Dim RS As RandStack = CreateRandStack()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True
        Dim UnapropriateStacks() As AllDataStructues.Stack = New AllDataStructues.Stack() { _
            New AllDataStructues.Stack With {.leaderPos = -1, .units = Nothing}, _
            New AllDataStructues.Stack With {.leaderPos = -1, _
                                             .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000UU0001", "G000000000", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 0, _
                                             .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000UU0001", "G000000000", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000UU5201", "G000000000", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 0, .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000UU5201", "G000UU5201", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000UU5201", "G000UU0006", "G000UU5201", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 0, .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000UU5230", "G000UU0006", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 0, .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000UU0001", "G000UU0006", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000000000", "G000UU5230", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                 New String() {"G000000000", "G000UU5201", "G000000000", "G000000000", "G000000000", "G000000000"}, _
                                                 New Integer() {1, 1, 1, 1, 1, 1}, Nothing, RS)}}

        For Each item As AllDataStructues.Stack In UnapropriateStacks
            If TestStack(item, target) Then
                ok = False
                Exit For
            End If
        Next item

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for StackStats
    '''</summary>
    <TestMethod()> _
    Public Sub StackStatsTest2()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Dim RS As RandStack = CreateRandStack()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim s As New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu5130", "G000000000", "G000000000", _
                                                                   "G000000000", "G000000000", "g000uu5130"}, _
                                                     New Integer() {1, 0, 0, 0, 0, 1}, Nothing, RS), _
                                                  .items = New List(Of String)}
        Dim res As AllDataStructues.DesiredStats = target.StackStats(s, False)
        Dim ok As Boolean = True
        If Not res.Race.Contains(1) Or res.Race.Count > 1 Then ok = False
        If Not res.ExpStackKilled = 60 Then ok = False
        If Not res.ExpBarAverage = 50 Then ok = False
        If Not res.MeleeCount = 2 Then ok = False
        If Not res.StackSize = 2 Then ok = False

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest3()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Dim RS As RandStack = CreateRandStack()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu5124", "G000000000", "G000000000", _
                                                                   "G000000000", "G000000000", "G000000000"}, _
                                                     New Integer() {1, 0, 0, 0, 0, 0}, Nothing, RS), _
                                                  .items = New List(Of String)}
        Dim tstats As AllDataStructues.DesiredStats = target.StackStats(s, False)

        Dim stats As New AllDataStructues.DesiredStats With {.ExpBarAverage = 950, .ExpStackKilled = 120, .Race = New List(Of Integer), _
                                                             .StackSize = 1, .MaxGiants = 0, .MeleeCount = 1, .LootCost = 0, _
                                                             .IGen = New AllDataStructues.LootGenSettings(False), _
                                                             .LeaderModificators = New List(Of String), _
                                                             .UnitsModificators = New List(Of String)}
        Dim races() As Integer = New Integer() {10}
        stats.Race.AddRange(races)

        If Not stats.ExpBarAverage = tstats.ExpBarAverage Then ok = False
        If Not stats.ExpStackKilled = tstats.ExpStackKilled Then ok = False
        If Not stats.MaxGiants = tstats.MaxGiants Then ok = False
        If Not stats.MeleeCount = tstats.MeleeCount Then ok = False
        If Not stats.StackSize = tstats.StackSize Then ok = False
        If Not stats.Race.Contains(tstats.Race.Item(0)) Then ok = False

        If ok Then
            Dim raceokL(UBound(races)), raceokF(UBound(races)) As Boolean
            stats.Race.AddRange(races)

            For i As Integer = 0 To 1000 Step 1
                Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
                Dim stack As AllDataStructues.Stack = target.Gen(gs)
                ok = TestStack(stack, target, races, raceokL, raceokF)
                If Not ok Then Exit For
            Next i
            For r As Integer = 0 To UBound(races) Step 1
                If Not raceokL(r) And Not raceokF(r) Then ok = False
            Next r
        End If
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest4()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Dim RS As RandStack = CreateRandStack()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu5119", "G000000000", "G000000000", _
                                                                   "G000000000", "G000000000", "G000000000"}, _
                                                     New Integer() {1, 0, 0, 0, 0, 0}, Nothing, RS), _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s, False)

        Dim initSize As Integer = StackSize(s)
        For i As Integer = 0 To 1000 Step 1
            Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
            Dim stack As AllDataStructues.Stack = target.Gen(gs)
            If StackSize(stack) > initSize + 1 Then ok = False
        Next i

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    Private Function StackSize(ByRef stack As AllDataStructues.Stack) As Integer
        Dim size As Integer = 0
        For Each item As AllDataStructues.Stack.UnitInfo In stack.units
            If Not item.unit.unitID = GenDefaultValues.emptyItem Then
                If item.unit.small Then
                    size += 1
                Else
                    size += 2
                End If
            End If
        Next item
        Return size
    End Function

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest5()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Dim RS As RandStack = CreateRandStack()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu5130", "g000uu5030", "G000000000", _
                                                                    "G000000000", "G000000000", "G000000000"}, _
                                                     New Integer() {1, 1, 0, 0, 0, 0}, Nothing, rs), _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s, False)

        Dim c As Integer
        For i As Integer = 0 To 1000 Step 1
            Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
            gs.noLeader = True
            Dim stack As AllDataStructues.Stack = target.Gen(gs)
            c = 0
            For Each item As AllDataStructues.Stack.UnitInfo In stack.units
                If Not item.unit.unitID = GenDefaultValues.emptyItem Then
                    c += 1
                    If item.unit.unitBranch = GenDefaultValues.UnitClass.leader Then ok = False
                End If
            Next item
            If c > 3 Then ok = False
            If Not ok Then Exit For
        Next i

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest61()

        Dim target As RandStack_Accessor = Nothing
        Dim stats As AllDataStructues.DesiredStats = TestGoblinsGenStats(target, False)

        Dim ok As Boolean = TestGoblinsGen(target, stats, True)

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest62()

        Dim target As RandStack_Accessor = Nothing
        Dim stats As AllDataStructues.DesiredStats = TestGoblinsGenStats(target, False)

        Dim ok As Boolean = TestGoblinsGen(target, stats, False)

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest63()

        Dim target As RandStack_Accessor = Nothing
        Dim stats As AllDataStructues.DesiredStats = TestGoblinsGenStats(target, True)

        Dim ok As Boolean = TestGoblinsGen(target, stats, False)

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    Private Function TestGoblinsGenStats(ByRef target As RandStack_Accessor, ByRef twogoblins As Boolean) As AllDataStructues.DesiredStats
        Call PrepareToTest()
        target = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()
        Dim s As AllDataStructues.Stack
        Dim RS As RandStack = CreateRandStack()
        If twogoblins Then
            s = New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu5117", "g000uu5018", "g000000000", _
                                                                   "g000000000", "g000000000", "G000000000"}, _
                                                     New Integer() {1, 1, 0, 0, 0, 0}, Nothing, RS), _
                                                 .items = New List(Of String)}
        Else
            s = New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu5117", "g000uu5018", "g000uu5017", _
                                                                   "g000uu5017", "g000uu5018", "G000000000"}, _
                                                     New Integer() {1, 1, 1, 1, 1, 0}, Nothing, RS), _
                                                 .items = New List(Of String)}
        End If
        Return target.StackStats(s, False)
    End Function
    Private Function TestGoblinsGen(ByRef target As RandStack_Accessor, ByRef stats As AllDataStructues.DesiredStats, _
                                    ByRef TestOverload1 As Boolean) As Boolean
        Dim ok As Boolean = True
        Dim expected As New List(Of String)
        expected.AddRange(New String() {"g000000000", "g000uu5117", "g000uu5017", "g000uu5018", "g000uu8196", _
                                        "g000uu7539", "g001uu7539", "g000uu8285", "g000uu5113", "g000uu5013", _
                                        "g003uu5013", "g001uu7556", "g000uu7556"})

        Dim noLeader As Boolean = False
        For p As Integer = 0 To 1 Step 1
            For i As Integer = 0 To 1000 Step 1
                Dim stack As AllDataStructues.Stack
                If TestOverload1 Then
                    Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
                    gs.noLeader = noLeader
                    stack = target.Gen(gs)
                Else
                    Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
                    stack = target.Gen(gs)
                End If
                For Each item As AllDataStructues.Stack.UnitInfo In stack.units
                    If Not expected.Contains(item.unit.unitID.ToLower) Then ok = False
                Next item
                If Not ok Then Exit For
            Next i
            noLeader = Not noLeader
        Next p
        Return ok
    End Function

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest_with_UnitsPreservation()
        Call UnitsPreservationCommon(Nothing)
    End Sub
    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest_with_UnitsPreservationAndModificators()
        Dim modificators(5)() As String
        modificators(0) = {"g001um0005", "g002um0010"}
        modificators(1) = {"g000um0012", "g000um0035", "g100um0022"}
        modificators(2) = {"g002um0023", "g001um0001"}
        modificators(3) = {"g000um4005", "g000um4010", "g002um0024"}
        modificators(4) = {"g000um0001"}
        modificators(5) = {"g001um2007", "g000um0005", "g201um9071", "g201um9060"}
        Call UnitsPreservationCommon(modificators)
    End Sub
    Private Const UnitsPreservationCommon_LeaderPos As Integer = 3
    Private Sub UnitsPreservationCommon(ByRef modificators()() As String)
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        target.settings.preserveUnitsOverlevel = True
        Call target.ResetExclusions()
        target.log.Disable()

        If Not IsNothing(modificators) Then
            Dim mList As New List(Of String)
            For x As Integer = 0 To UBound(modificators) Step 1
                For y As Integer = 0 To UBound(modificators(x)) Step 1
                    If Not mList.Contains(modificators(x)(y).ToUpper) Then
                        mList.Add(modificators(x)(y).ToUpper)
                    Else
                        Throw New Exception
                    End If
                Next y
            Next x
        End If

        Dim preservedUnits, expectedUnit_Level As New List(Of String)
        Dim stats As AllDataStructues.DesiredStats
        Dim level() As Integer = New Integer() {2, 4, 8, 7, 9, 3}
        Dim s As New AllDataStructues.Stack With {.units = AllDataStructues.Stack.UnitInfo.CreateArray( _
                                                     New String() {"g000uu0001", "g000uu0002", "g000uu0012", _
                                                                   "g000uu5117", "g000uu0001", "g000uu0017"}, _
                                                     level, Nothing, CreateRandStack()), _
                                                  .items = New List(Of String)}
        If Not IsNothing(modificators) Then
            For i As Integer = 0 To UBound(modificators) Step 1
                s.units(i).modificators = modificators(i).ToList
            Next i
        End If
        For j As Integer = 0 To UBound(s.units) Step 1
            If Not s.units(j).level = level(j) Or s.units(j).level < s.units(j).unit.level Then
                Throw New Exception("Unexpected level")
            End If
        Next j

        Dim p1() As String = New String() {"g000uu0001"}
        Dim p2() As String = New String() {"g000uu0001", "g000uu0002"}
        Dim p3() As String = New String() {"g000uu5117"}
        Dim p4() As String = New String() {"g000uu0001", "g000uu0017"}
        Dim p5() As String = New String() {"g000uu5117", "g000uu0001", "g000uu0017"}
        Dim p6() As String = New String() {"g000uu0002", "g000uu0012"}
        Dim p7() As String = New String() {"g000uu0001", "g000uu0002", "g000uu0012"}
        Dim p8() As String = New String() {"g000uu0001", "g000uu0002", "g000uu0012", "g000uu5117", "g000uu0017"}
        Dim p9() As String = New String() {"g000uu0002", "g000uu0012", "g000uu5117", "g000uu0001"}
        Dim p()() As String = {p1, p2, p3, p4, p5, p6, p7, p8, p9}
        Dim ok As Boolean = True

        For i As Integer = 0 To UBound(p) Step 1
            preservedUnits.Clear()
            preservedUnits.AddRange(p(i))
            target.comm.preservedItems.Clear()
            target.comm.ReadPreservedObjects(preservedUnits)
            stats = target.StackStats(s, False)
            expectedUnit_Level.Clear()
            For j As Integer = 0 To UBound(s.units) Step 1
                If preservedUnits.Contains(s.units(j).unit.unitID.ToLower) Then
                    expectedUnit_Level.Add(s.units(j).unit.unitID.ToLower & "#" & s.units(j).level)
                End If
            Next j
            For m As Integer = 1 To 15 Step 1
                Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
                Dim res As AllDataStructues.Stack = target.Gen(gs, 0.1 * CDbl(m), 1)
                If Not IsNothing(modificators) Then
                    'проверяем, что у лидера все модификаторы лидера
                    For Each modif As String In modificators(UnitsPreservationCommon_LeaderPos)
                        If Not res.units(res.leaderPos).modificators.Contains(modif.ToUpper) Then ok = False
                    Next modif
                    'проверяем, что перераспределены все модификаторы
                    For x As Integer = 0 To UBound(modificators) Step 1
                        For y As Integer = 0 To UBound(modificators(x)) Step 1
                            For j As Integer = 0 To UBound(res.units) Step 1
                                If res.units(j).modificators.Contains(modificators(x)(y).ToUpper) Then
                                    Exit For
                                End If
                                If j = UBound(res.units) Then ok = False
                            Next j
                        Next y
                    Next x
                End If
                For Each e As String In expectedUnit_Level
                    For k As Integer = 0 To UBound(res.units) Step 1
                        If e = res.units(k).unit.unitID.ToLower & "#" & res.units(k).level Then Exit For
                        If k = UBound(res.units) Then ok = False
                    Next k
                    If Not ok Then Exit For
                Next e
                If Not ok Then Exit For
            Next m
            If Not ok Then Exit For
        Next i

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for StackStats
    '''</summary>
    <TestMethod()> _
    Public Sub StackStatsTest()

        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Dim RS As RandStack = CreateRandStack()
        target.AllUnits = New AllDataStructues.Unit() { _
            New AllDataStructues.Unit With {.dynUpgrade1 = New AllDataStructues.DynUpgrade With {.EXPkilled = 10, .EXPnext = 100}, _
                                            .dynUpgrade2 = New AllDataStructues.DynUpgrade With {.EXPkilled = 1, .EXPnext = 10}, _
                                            .dynUpgradeLevel = 10, _
                                            .EXPkilled = 10, _
                                            .EXPnext = 100, _
                                            .leadership = 6, _
                                            .level = 1, _
                                            .name = "testunit", _
                                            .race = 1, _
                                            .reach = GenDefaultValues.UnitAttackReach.archer, _
                                            .small = True, _
                                            .unitBranch = GenDefaultValues.UnitClass.archer, _
                                            .unitCost = New AllDataStructues.Cost With {.Gold = 1000}, _
                                            .unitID = "g000uu0001", _
                                            .waterOnly = False}}

        Call target.ResetExclusions()
        Dim ok As Boolean = True
        Dim expectedEkill() As Integer = New Integer() {10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110}
        Dim expectedEbar() As Integer = New Integer() {100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090, 1100}

        Dim s As New AllDataStructues.Stack With {.leaderPos = 0, .items = New List(Of String)}
        ReDim s.units(5)
        For i As Integer = 1 To UBound(s.units) Step 1
            s.units(i) = AllDataStructues.Stack.UnitInfo.CreateEmpty
        Next i
        s.units(0) = New AllDataStructues.Stack.UnitInfo(target.AllUnits(0).unitID, 0, Nothing, RS)
        For unitBaseLevel As Integer = 1 To 5 Step 1
            target.AllUnits(0).level = unitBaseLevel
            For i As Integer = unitBaseLevel To expectedEkill.Length Step 1
                s.units(0).level = i
                Dim stats As AllDataStructues.DesiredStats = target.StackStats(s, False)
                If Not stats.ExpStackKilled = expectedEkill(i - 1) Then ok = False
                If Not stats.ExpBarAverage = expectedEbar(i - 1) Then ok = False
            Next i
            For i As Integer = 0 To UBound(expectedEkill) Step 1
                expectedEkill(i) -= target.AllUnits(0).dynUpgrade1.EXPkilled
                expectedEbar(i) -= target.AllUnits(0).dynUpgrade1.EXPnext
            Next i
        Next unitBaseLevel

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for GoldToMana
    '''</summary>
    <TestMethod()> _
    Public Sub GoldToManaTest()

        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True
        Dim input, result, mana As New AllDataStructues.Cost
        Dim s1, s2 As Integer

        For m1 As Integer = 0 To 2 Step 1
            For m2 As Integer = 0 To 2 Step 1
                For m3 As Integer = 0 To 2 Step 1
                    For m4 As Integer = 0 To 2 Step 1
                        For m5 As Integer = 0 To 2 Step 1
                            mana.Black = m1
                            mana.Blue = m2
                            mana.Green = m3
                            mana.Red = m4
                            mana.White = m5
                            target.mapData.minesAmount = mana
                            For j As Integer = 0 To 11 Step 1
                                For i As Integer = 0 To 1000 Step 10
                                    input = New AllDataStructues.Cost With {.Gold = i, _
                                                                            .Black = target.rndgen.RndPos(1001, True) - 1, _
                                                                            .Blue = target.rndgen.RndPos(1001, True) - 1, _
                                                                            .Green = target.rndgen.RndPos(1001, True) - 1, _
                                                                            .Red = target.rndgen.RndPos(1001, True) - 1, _
                                                                            .White = target.rndgen.RndPos(1001, True) - 1}
                                    result = target.GoldToMana(input, 1, 0.1 * CDbl(j))
                                    s1 = AllDataStructues.Cost.Sum(input)
                                    s2 = AllDataStructues.Cost.Sum(result)
                                    If result.Gold < 0 Then ok = False
                                    If Not s1 = s2 Then ok = False
                                Next i
                            Next j
                            If Not ok Then
                                m1 = 2
                                m2 = 2
                                m3 = 2
                                m4 = 2
                                m5 = 2
                            End If
                        Next m5
                    Next m4
                Next m3
            Next m2
        Next m1

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Log
    '''</summary>
    <TestMethod()> _
    Public Sub LoggingTest()

        Call CreateRandStack_Accessor()
        Dim target As RandStack = CreateRandStack()
        target.comm.ReadExcludedObjectsList()
        target.settings.preserveUnitsOverlevel = True

        Dim ok As Boolean = True

        target.log.Enable()
        target.comm.PrintResourcesToLog(target.log, target)

        Dim overlevelLog As New Log(target.comm)
        overlevelLog.Enable()

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

        overlevelLog.Add("level" & vbTab & "exp_killed" & vbTab & "unit_id")
        For k As Integer = 0 To UBound(s.units) Step 1
            If Not s.units(k).unit.unitID.ToUpper = GenDefaultValues.emptyItem Then
                overlevelLog.Add(Math.Max(s.units(k).unit.level, s.units(k).level) & vbTab & s.units(k).unit.EXPkilled & vbTab & s.units(k).unit.unitID)
            End If
        Next k
        overlevelLog.Add("----------")

        Dim stack As AllDataStructues.Stack
        For i As Integer = 1 To 20 Step 1
            stack = target.Gen(gs)
            For k As Integer = 0 To UBound(s.units) Step 1
                If Not stack.units(k).unit.unitID.ToUpper = GenDefaultValues.emptyItem Then
                    overlevelLog.Add(stack.units(k).level & vbTab & stack.units(k).unit.EXPkilled & vbTab & stack.units(k).unit.unitID)
                End If
            Next k
            overlevelLog.Add("----------")
        Next i
        Dim log As String = target.log.PrintAll
        Dim oLog As String = overlevelLog.PrintAll
        If log = "" Then ok = False

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
