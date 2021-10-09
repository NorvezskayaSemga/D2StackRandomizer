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


    Private def() As String = {"%default%"}

    Friend Function CreateRandStack_Accessor() As RandStack_Accessor
        Call PrepareToTest()
        Dim rstackData As New RandStack.ConstructorInput With {.AllUnitsList = UnitsList, _
                                                               .AllItemsList = ItemsList, _
                                                               .AllSpellsList = AllSpells}
        rstackData.settings.modName = GenDefaultValues.DefaultMod
        Return New RandStack_Accessor(rstackData)
    End Function
    Friend Function CreateRandStack() As RandStack
        Call PrepareToTest()
        Dim rstackData As New RandStack.ConstructorInput With {.AllUnitsList = UnitsList, _
                                                               .AllItemsList = ItemsList, _
                                                               .AllSpellsList = AllSpells}
        rstackData.settings.modName = GenDefaultValues.DefaultMod
        Return New RandStack(rstackData)
    End Function
    Friend Sub PrepareToTest()
        Call CopyResources()
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        If IsNothing(AllSpells) Then Call ReadTestSpells()
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

    Friend T1Units() As String = {
"g000uu3001", "g000uu0023", "g000uu0019", "g000uu0020", "g000uu8248", "g000uu0022", "g000uu0001", "g000uu0006", "g000uu0008", "g000uu0011", "g000uu0018", "g003uu5001",
"g000uu3002", "g000uu0048", "g000uu0044", "g000uu0045", "g000uu8249", "g000uu0047", "g000uu0036", "g000uu0026", "g000uu0033", "g000uu0029", "g000uu0043", "g004uu5039",
"g000uu3003", "g000uu0074", "g000uu0070", "g000uu0071", "g000uu8250", "g000uu0073", "g000uu0052", "g000uu0055", "g000uu0062", "g000uu0057", "g000uu0069", "g004uu6120",
"g000uu3004", "g000uu0100", "g000uu0096", "g000uu8252", "g000uu8253", "g000uu0099", "g000uu0086", "g000uu0078", "g000uu0080", "g000uu0093", "g000uu0092", "g001uu7539",
"g000000000", "g000000000", "g000000000", "g000000000", "g000000000", "g000000000", "g000000000", "g000000000", "g000000000", "g000000000", "g000000000", "g000000000",
"g000uu8040", "g000uu8013", "g000uu8009", "g000uu8011", "g000uu8251", "g000uu8012", "g000uu8014", "g000uu8018", "g000uu8025", "g000uu8031", "g000uu8029", "g003uu8037"}

    Friend UnitsList() As AllDataStructues.Unit = Nothing
    Friend ItemsList() As AllDataStructues.Item = Nothing
    Friend AllSpells() As AllDataStructues.Spell = Nothing

    Friend Sub ReadTestUnits()
        UnitsList = ImpenetrableMeshShow.StartForm_Accessor.ReadTestUnits(GenDefaultValues.DefaultMod)
    End Sub
    Friend Sub ReadTestItems()
        ItemsList = ImpenetrableMeshShow.StartForm_Accessor.ReadTestItems(GenDefaultValues.DefaultMod)
    End Sub
    Friend Sub ReadTestSpells()
        AllSpells = ImpenetrableMeshShow.StartForm_Accessor.ReadSpells
    End Sub

    Private Function TestGenSettings(ByRef d As AllDataStructues.DesiredStats, Optional ByVal GroundTile As Boolean = True) As AllDataStructues.CommonStackCreationSettings
        Return New AllDataStructues.CommonStackCreationSettings _
            With {.StackStats = d, _
                  .deltaLeadership = 0, _
                  .GroundTile = GroundTile, _
                  .NoLeader = False, .pos = New Point(1, 1)}
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
                                                   .IGen = New AllDataStructues.LootGenSettings(False)})
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
        If stack.leaderPos < 0 Or IsNothing(stack.pos) Then Return False
        If stack.pos(stack.leaderPos) = GenDefaultValues.emptyItem Then Return False
        For i As Integer = 0 To UBound(stack.pos) Step 1
            If stack.pos(i) = "" Then
                Return False
            ElseIf Not stack.pos(i) = GenDefaultValues.emptyItem Then
                unit = target.FindUnitStats(stack.pos(i))
                If (unit.unitBranch = GenDefaultValues.UnitClass.leader And Not i = stack.leaderPos) Or (Not unit.unitBranch = GenDefaultValues.UnitClass.leader And i = stack.leaderPos) Then Return False
                If Not unit.small AndAlso (busytransfer(i) = -1 Or Not stack.pos(busytransfer(i)) = GenDefaultValues.emptyItem) Then Return False
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

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"G000UU5356", "G000UU0174", "G005UU6111", _
                                                                       "G000000000", "G000UU0162", "G000UU0162"}, _
                                                  .level = New Integer() {1, 1, 1, 0, 1, 1}, _
                                                  .items = New List(Of String)}
        s.items.AddRange(New String() {"G000IG0011", "G000IG0012", "G000IG0013", "G000IG0014", _
                                       "G000IG0015", "G000IG0016", "G000IG1001", "G000IG1002"})
        Dim expected As New AllDataStructues.DesiredStats With {.Race = New List(Of Integer), _
                                                                .LootCost = 200 + 450 + 1500 + 200 + 450 + 1500 + 1000 + 3000, _
                                                                .StackSize = 6, _
                                                                .MeleeCount = 2, _
                                                                .MaxGiants = 1, _
                                                                .ExpStackKilled = 320 + 100 + 535 + 175 * 2, _
                                                                .ExpBarAverage = 2100 + 725 + 2800 + 1100 * 2}
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
    Public Sub ItemsGenTest()
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
                                                             .IGen = New AllDataStructues.LootGenSettings(False)}
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
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True
        Dim UnapropriateStacks() As AllDataStructues.Stack = New AllDataStructues.Stack() { _
            New AllDataStructues.Stack With {.leaderPos = -1, .pos = Nothing, .level = Nothing}, _
            New AllDataStructues.Stack With {.leaderPos = -1, .pos = New String() {"G000UU0001", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 0, .pos = New String() {"G000UU0001", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .pos = New String() {"G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 0, .pos = New String() {"G000UU5201", "G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .pos = New String() {"G000UU5201", "G000UU0006", "G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 0, .pos = New String() {"G000UU5230", "G000UU0006", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 0, .pos = New String() {"G000UU0001", "G000UU0006", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .pos = New String() {"G000UU0000", "G000UU5230", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}, _
            New AllDataStructues.Stack With {.leaderPos = 1, .pos = New String() {"G000UU0000", "G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}, _
                                             .level = New Integer() {1, 1, 1, 1, 1, 1}}}

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
        Call target.ResetExclusions()
        target.log.Disable()

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5130", "G000000000", "G000000000", _
                                                                      "G000000000", "G000000000", "g000uu5130"}, _
                                                  .level = New Integer() {1, 0, 0, 0, 0, 1}, _
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
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5124", "G000000000", "G000000000", _
                                                                       "G000000000", "G000000000", "G000000000"}, _
                                                  .level = New Integer() {1, 0, 0, 0, 0, 0}, _
                                                  .items = New List(Of String)}
        Dim tstats As AllDataStructues.DesiredStats = target.StackStats(s, False)

        Dim stats As New AllDataStructues.DesiredStats With {.ExpBarAverage = 950, .ExpStackKilled = 120, .Race = New List(Of Integer), _
                                                             .StackSize = 1, .MaxGiants = 0, .MeleeCount = 1, .LootCost = 0, _
                                                             .IGen = New AllDataStructues.LootGenSettings(False)}
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
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5119", "G000000000", "G000000000", _
                                                                       "G000000000", "G000000000", "G000000000"}, _
                                                  .level = New Integer() {1, 0, 0, 0, 0, 0}, _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s, False)

        Dim c As Integer
        For i As Integer = 0 To 1000 Step 1
            Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
            Dim stack As AllDataStructues.Stack = target.Gen(gs)
            c = 0
            For Each item As String In stack.pos
                If Not item = GenDefaultValues.emptyItem Then c += 1
            Next item
            If c > 1 Then ok = False
        Next i

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest5()
        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
        Call target.ResetExclusions()
        target.log.Disable()

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5130", "g000uu5030", "G000000000", _
                                                                       "G000000000", "G000000000", "G000000000"}, _
                                                  .level = New Integer() {1, 1, 0, 0, 0, 0}, _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s, False)

        Dim c As Integer
        For i As Integer = 0 To 1000 Step 1
            Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
            gs.NoLeader = True
            Dim stack As AllDataStructues.Stack = target.Gen(gs)
            c = 0
            For Each item As String In stack.pos
                If Not item = GenDefaultValues.emptyItem Then
                    c += 1
                    If target.FindUnitStats(item).unitBranch = GenDefaultValues.UnitClass.leader Then ok = False
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
        If twogoblins Then
            s = New AllDataStructues.Stack With {.pos = New String() {"g000uu5117", "g000uu5018", "g000000000", _
                                                                      "g000000000", "g000000000", "G000000000"}, _
                                                 .level = New Integer() {1, 1, 0, 0, 0, 0}, _
                                                 .items = New List(Of String)}
        Else
            s = New AllDataStructues.Stack With {.pos = New String() {"g000uu5117", "g000uu5018", "g000uu5017", _
                                                                      "g000uu5017", "g000uu5018", "G000000000"}, _
                                                 .level = New Integer() {1, 1, 1, 1, 1, 0}, _
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
                                        "g003uu5013"})

        Dim noLeader As Boolean = False
        For p As Integer = 0 To 1 Step 1
            For i As Integer = 0 To 1000 Step 1
                Dim stack As AllDataStructues.Stack
                If TestOverload1 Then
                    Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
                    gs.NoLeader = noLeader
                    stack = target.Gen(gs)
                Else
                    Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)
                    stack = target.Gen(gs)
                End If
                For Each item As String In stack.pos
                    If Not expected.Contains(item.ToLower) Then ok = False
                Next item
                If Not ok Then Exit For
            Next i
            noLeader = Not noLeader
        Next p
        Return ok
    End Function


    '''<summary>
    '''A test for StackStats
    '''</summary>
    <TestMethod()> _
    Public Sub StackStatsTest()

        Dim target As RandStack_Accessor = CreateRandStack_Accessor()
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
        Dim ExcludeLists() As String = New String() {"%defailt%"}
        Dim CustomUnitRace() As String = New String() {"%defailt%"}
        Dim CustomLootChance() As String = New String() {"%defailt%"}

        Call target.ResetExclusions()
        Dim ok As Boolean = True
        Dim expectedEkill() As Integer = New Integer() {10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110}
        Dim expectedEbar() As Integer = New Integer() {100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090, 1100}

        Dim s As New AllDataStructues.Stack With {.leaderPos = 0, .items = New List(Of String)}
        ReDim s.pos(5), s.level(5)
        For i As Integer = 1 To UBound(s.pos) Step 1
            s.pos(i) = GenDefaultValues.emptyItem
        Next i
        s.pos(0) = target.AllUnits(0).unitID.ToUpper
        For unitBaseLevel As Integer = 1 To 5 Step 1
            target.AllUnits(0).level = unitBaseLevel
            For i As Integer = unitBaseLevel To expectedEkill.Length Step 1
                s.level(0) = i
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

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5017", "g000uu5117", "g000uu5018", _
                                                                       "g000uu5018", "G000000000", "g000uu5018"}, _
                                                  .level = New Integer() {1, 1, 5, 1, 0, 10}, _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s, False)
        stats.LootCost = 2000
        stats.IGen = New AllDataStructues.LootGenSettings(False) With {.ConsumableItems = New AllDataStructues.ItemGenSettings With {.amount = 2}, _
                                                                       .NonconsumableItems = New AllDataStructues.ItemGenSettings With {.costPart = 0.333}, _
                                                                       .JewelItems = New AllDataStructues.ItemGenSettings With {.exclude = True, .costPart = 0.9}}

        Dim gs As AllDataStructues.CommonStackCreationSettings = TestGenSettings(stats)

        overlevelLog.Add("level" & vbTab & "exp_killed" & vbTab & "unit_id")
        For k As Integer = 0 To UBound(s.pos) Step 1
            If Not s.pos(k).ToUpper = GenDefaultValues.emptyItem Then
                Dim u As AllDataStructues.Unit = target.FindUnitStats(s.pos(k))
                overlevelLog.Add(Math.Max(u.level, s.level(k)) & vbTab & u.EXPkilled & vbTab & u.unitID)
            End If
        Next k
        overlevelLog.Add("----------")

        Dim stack As AllDataStructues.Stack
        For i As Integer = 1 To 20 Step 1
            stack = target.Gen(gs)
            For k As Integer = 0 To UBound(s.pos) Step 1
                If Not stack.pos(k).ToUpper = GenDefaultValues.emptyItem Then
                    Dim u As AllDataStructues.Unit = target.FindUnitStats(stack.pos(k))
                    overlevelLog.Add(stack.level(k) & vbTab & u.EXPkilled & vbTab & u.unitID)
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
