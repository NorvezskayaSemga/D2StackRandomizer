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

    Friend excludeList() As String = New String() {RandomStackGenerator.My.Resources.readDefaultFileKeyword, _
                                                   RandomStackGenerator.My.Resources.readDefaultFileKeyword}
    Friend customRaceList() As String = New String() {RandomStackGenerator.My.Resources.readDefaultFileKeyword, _
                                                      RandomStackGenerator.My.Resources.readDefaultFileKeyword}
    Friend UnitsList() As AllDataStructues.Unit = Nothing
    Friend ItemsList() As AllDataStructues.Item = Nothing
    Friend AllSpells As Dictionary(Of String, AllDataStructues.Spell) = Nothing

    Friend Sub ReadTestUnits()
        Dim comm As New Common
        Dim s() As String = comm.TxtSplit(RandomStackGenerator.My.Resources.TestUnitsTable)
        Dim r() As String
        ReDim UnitsList(UBound(s) - 1)
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
            End If
        Next i
    End Sub
    Friend Sub ReadTestItems()
        Dim comm As New Common
        Dim s() As String = comm.TxtSplit(RandomStackGenerator.My.Resources.TestItemsTable)
        Dim r() As String
        ReDim ItemsList(UBound(s) - 1)
        For i As Integer = 1 To UBound(s) Step 1
            r = s(i).Split(" ")
            If r.Length = 3 Then
                ItemsList(i - 1).type = r(0)
                ItemsList(i - 1).itemID = r(1)
                ItemsList(i - 1).itemCost = AllDataStructues.Cost.Read(r(2))
            End If
        Next i
    End Sub
    Friend Sub ReadTestSpells()
        Dim s As New ImpenetrableMeshShow.StartForm_Accessor
        AllSpells = s.ReadSpells
    End Sub

    '''<summary>
    '''A test for RndPos
    '''</summary>
    <TestMethod(), _
     DeploymentItem("RandomStackGenerator.dll")> _
    Public Sub RndPosTest()
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)

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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As Common_Accessor = New Common_Accessor()
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)

        Dim ok As Boolean = True
        Dim races() As String = New String() {"H", "U", "L", "C", "E", "N", "G", "D", "S", "W", "B", "A", "AS", "AST", "AW"}

        Parallel.For(0, races.Length, _
         Sub(r As Integer)
             If Not ok Then Exit Sub
             Dim rList As New List(Of Integer)
             rList.Add(target.comm.RaceIdentifierToSubrace(races(r)))
             Dim stats As New AllDataStructues.DesiredStats
             For eb As Integer = 0 To 10 Step 1
                 stats.ExpBarAverage = 100 + 200 * eb
                 For ek As Integer = 0 To 4 Step 1
                     stats.ExpStackKilled = 200 + 50 * ek
                     For mg As Integer = 0 To 3 Step 1
                         stats.MaxGiants = mg
                         For mc As Integer = 0 To 3 Step 1
                             stats.MeleeCount = mc
                             For s As Integer = 1 To 6 Step 1
                                 stats.StackSize = s
                                 stats.Race = rList

                                 Dim GroundTile As Boolean
                                 For g As Integer = 0 To 1 Step 1
                                     If g = 0 Then
                                         GroundTile = False
                                         If races(r) = "W" Then g = 1
                                     Else
                                         GroundTile = True
                                     End If

                                     Dim stack As AllDataStructues.Stack = target.Gen(stats, GroundTile, False)
                                     ok = TestStack(stack, target)
                                     If Not ok Then Exit For
                                 Next g
                                 If Not ok Then Exit For
                             Next s
                             If Not ok Then Exit For
                         Next mc
                         If Not ok Then Exit For
                     Next mg
                     If Not ok Then Exit For
                 Next ek
                 If Not ok Then Exit For
             Next eb
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
        If stack.pos(stack.leaderPos) = target.emptyItem Then Return False
        For i As Integer = 0 To UBound(stack.pos) Step 1
            If stack.pos(i) = "" Then
                Return False
            ElseIf Not stack.pos(i) = target.emptyItem Then
                unit = target.FindUnitStats(stack.pos(i))
                If (unit.unitBranch = 5 And Not i = stack.leaderPos) Or (Not unit.unitBranch = 5 And i = stack.leaderPos) Then Return False
                If Not unit.small AndAlso (busytransfer(i) = -1 Or Not stack.pos(busytransfer(i)) = target.emptyItem) Then Return False
                If unit.reach = 3 Then
                    For j As Integer = 0 To UBound(secondrow) Step 1
                        If i = secondrow(j) Then Return False
                    Next j
                End If
                If Not IsNothing(races) Then
                    For r As Integer = 0 To UBound(races) Step 1
                        If unit.race = races(r) Then
                            If unit.unitBranch = 5 Then
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)
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
        Dim calculated As AllDataStructues.DesiredStats = target.StackStats(s)
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)
        Dim ok As Boolean = True
        Dim cost, sum As Integer
        Dim genitems As List(Of String)
        For i As Integer = 0 To 100 Step 1
            cost = target.minItemGoldCost + i * 125
            genitems = target.ItemsGen(cost, False, False)
            sum = 0
            For Each id As String In genitems
                If target.itemType.Item(target.FindItemStats(id).type) = "JEWEL" Then
                    sum += target.FindItemStats(id).itemCost.Gold / 2
                Else
                    sum += target.FindItemStats(id).itemCost.Gold
                End If
            Next id
            If sum > cost Then ok = False
            If Math.Abs(cost - sum) >= target.minItemGoldCost Then ok = False
            If Not ok Then Exit For
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Gen
    '''</summary>
    <TestMethod()> _
    Public Sub GenTest2()
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)

        Dim ok As Boolean = True

        Dim stats As New AllDataStructues.DesiredStats With {.ExpBarAverage = 1450, .ExpStackKilled = 1000, .Race = New List(Of Integer), _
                                                      .StackSize = 3, .MaxGiants = 1, .MeleeCount = 3, .LootCost = 1200}
        Dim races() As Integer = New Integer() {1, 2}
        Dim raceokL(UBound(races)), raceokF(UBound(races)) As Boolean
        stats.Race.AddRange(races)

        For i As Integer = 0 To 10000 Step 1
            Dim stack As AllDataStructues.Stack = target.Gen(stats, True, False)
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)

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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()

        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)
        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5130", "G000000000", "G000000000", _
                                                                       "G000000000", "G000000000", "g000uu5130"}, _
                                                  .level = New Integer() {1, 0, 0, 0, 0, 1}, _
                                                  .items = New List(Of String)}
        Dim res As AllDataStructues.DesiredStats = target.StackStats(s)
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5124", "G000000000", "G000000000", _
                                                                       "G000000000", "G000000000", "G000000000"}, _
                                                  .level = New Integer() {1, 0, 0, 0, 0, 0}, _
                                                  .items = New List(Of String)}
        Dim tstats As AllDataStructues.DesiredStats = target.StackStats(s)

        Dim stats As New AllDataStructues.DesiredStats With {.ExpBarAverage = 950, .ExpStackKilled = 120, .Race = New List(Of Integer), _
                                                      .StackSize = 1, .MaxGiants = 0, .MeleeCount = 1, .LootCost = 0}
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

            For i As Integer = 0 To 10000 Step 1
                Dim stack As AllDataStructues.Stack = target.Gen(stats, True, False)
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5119", "G000000000", "G000000000", _
                                                                       "G000000000", "G000000000", "G000000000"}, _
                                                  .level = New Integer() {1, 0, 0, 0, 0, 0}, _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s)

        Dim c As Integer
        For i As Integer = 0 To 10000 Step 1
            Dim stack As AllDataStructues.Stack = target.Gen(stats, True, False)
            c = 0
            For Each item As String In stack.pos
                If Not item = "G000000000" Then c += 1
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)

        Dim ok As Boolean = True

        Dim s As New AllDataStructues.Stack With {.pos = New String() {"g000uu5130", "g000uu5030", "G000000000", _
                                                                       "G000000000", "G000000000", "G000000000"}, _
                                                  .level = New Integer() {1, 1, 0, 0, 0, 0}, _
                                                  .items = New List(Of String)}
        Dim stats As AllDataStructues.DesiredStats = target.StackStats(s)

        Dim c As Integer
        For i As Integer = 0 To 10000 Step 1
            Dim stack As AllDataStructues.Stack = target.Gen(stats, True, True)
            c = 0
            For Each item As String In stack.pos
                If Not item = "G000000000" Then
                    c += 1
                    If target.FindUnitStats(item).unitBranch = 5 Then ok = False
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
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        target = New RandStack_Accessor(UnitsList, ItemsList, excludeList, customRaceList, False)
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
        Return target.StackStats(s)
    End Function
    Private Function TestGoblinsGen(ByRef target As RandStack_Accessor, ByRef stats As AllDataStructues.DesiredStats, _
                                    ByRef TestOverload1 As Boolean) As Boolean
        Dim ok As Boolean = True
        Dim expected As New List(Of String)
        expected.AddRange(New String() {"g000000000", "g000uu5117", "g000uu5017", "g000uu5018", "g000uu8196"})

        If Not TestOverload1 Then expected.AddRange(New String() {"g000uu7539", "g001uu7539"})

        Dim noLeader As Boolean = False
        For p As Integer = 0 To 1 Step 1
            For i As Integer = 0 To 10000 Step 1
                Dim stack As AllDataStructues.Stack
                If TestOverload1 Then
                    stack = target.Gen(stats, True, noLeader)
                Else
                    stack = target.Gen(stats.ExpStackKilled, stats.LootCost, stats.Race, stats.excludeConsumableItems, stats.excludeNonconsumableItems, True, False)
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
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim AllUnitsList() As AllDataStructues.Unit = New AllDataStructues.Unit() { _
            New AllDataStructues.Unit With {.dynUpgrade1 = New AllDataStructues.DynUpgrade With {.EXPkilled = 10, .EXPnext = 100}, _
                                            .dynUpgrade2 = New AllDataStructues.DynUpgrade With {.EXPkilled = 1, .EXPnext = 10}, _
                                            .dynUpgradeLevel = 10, _
                                            .EXPkilled = 10, _
                                            .EXPnext = 100, _
                                            .leadership = 6, _
                                            .level = 1, _
                                            .name = "testunit", _
                                            .race = 1, _
                                            .reach = 2, _
                                            .small = True, _
                                            .unitBranch = 1, _
                                            .unitCost = New AllDataStructues.Cost With {.Gold = 1000}, _
                                            .unitID = "g000uu0001", _
                                            .waterOnly = False}}
        Dim ExcludeLists() As String = New String() {"%defailt%"}
        Dim CustomUnitRace() As String = New String() {"%defailt%"}
        Dim serial As Boolean = True

        Dim target As RandStack = New RandStack(AllUnitsList, ItemsList, ExcludeLists, CustomUnitRace, serial)
        Dim ok As Boolean = True
        Dim expectedEkill() As Integer = New Integer() {10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110}
        Dim expectedEbar() As Integer = New Integer() {100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080, 1090, 1100}

        Dim s As New AllDataStructues.Stack With {.leaderPos = 0, .items = New List(Of String)}
        ReDim s.pos(5), s.level(5)
        For i As Integer = 1 To UBound(s.pos) Step 1
            s.pos(i) = "G000000000"
        Next i
        s.pos(0) = AllUnitsList(0).unitID.ToUpper

        For i As Integer = 1 To expectedEkill.Length Step 1
            s.level(0) = i
            Dim stats As AllDataStructues.DesiredStats = target.StackStats(s)
            If Not stats.ExpStackKilled = expectedEkill(i - 1) Then ok = False
            If Not stats.ExpBarAverage = expectedEbar(i - 1) Then ok = False
        Next i

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
