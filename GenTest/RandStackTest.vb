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

    Dim excludeList() As String = New String() {"%default%", "%default%"}
    Dim UnitsList() As RandStack.Unit = Nothing
    Dim ItemsList() As RandStack.Item = Nothing
    Private Sub ReadTestUnits()
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
    Private Sub ReadTestItems()
        Dim comm As New Common
        Dim s() As String = comm.TxtSplit(RandomStackGenerator.My.Resources.TestItemsTable)
        Dim r() As String
        ReDim ItemsList(UBound(s) - 1)
        For i As Integer = 1 To UBound(s) Step 1
            r = s(i).Split(" ")
            If r.Length = 3 Then
                ItemsList(i - 1).Type = r(0)
                ItemsList(i - 1).itemID = r(1)
                ItemsList(i - 1).itemCost = RandStack.Cost.Read(r(2))
            End If
        Next i
    End Sub

    '''<summary>
    '''A test for RndPos
    '''</summary>
    <TestMethod(), _
     DeploymentItem("RandomStackGenerator.dll")> _
    Public Sub RndPosTest()
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, False)

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
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, False)

        Dim ok As Boolean = True

        Dim stats As New RandStack.DesiredStats
        Dim races() As String = New String() {"H", "U", "L", "C", "E", "N", "G", "D", "S", "W", "B", "A"}
        Dim rList As New List(Of Integer)
        Dim GroundTile As Boolean

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
                            For r As Integer = 0 To UBound(races) Step 1
                                rList.Clear()
                                rList.Add(target.comm.RaceIdentifierToSubrace(races(r)))
                                stats.Race = rList

                                For g As Integer = 0 To 1 Step 1
                                    If g = 0 Then
                                        GroundTile = False
                                        If races(r) = "W" Then g = 1
                                    Else
                                        GroundTile = True
                                    End If

                                    Dim stack As RandStack.Stack = target.Gen(stats, GroundTile)
                                    ok = TestStack(stack, target)
                                    If Not ok Then GoTo exittest

                                Next g
                            Next r
                        Next s
                    Next mc
                Next mg
            Next ek
        Next eb
exittest:
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    Private Function TestStack(ByRef stack As RandStack.Stack, ByRef target As RandStack_Accessor, _
                               Optional ByRef races() As Integer = Nothing, _
                               Optional ByRef raceokL() As Boolean = Nothing, _
                               Optional ByRef raceokF() As Boolean = Nothing) As Boolean
        Dim busytransfer() As Integer = New Integer() {1, -1, 3, -1, 5, -1}
        Dim firstrow() As Integer = New Integer() {0, 2, 4}
        Dim secondrow() As Integer = New Integer() {1, 3, 5}
        Dim unit As RandStack.Unit
        If stack.leaderPos < 0 Or IsNothing(stack.pos) Then
            Return False
        End If
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
    Public Sub StackStatsTest()
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        If IsNothing(ItemsList) Then Call ReadTestItems()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, False)
        Dim s As New RandStack.Stack With {.pos = New String() {"G000UU5356", "G000UU0174", "G005UU6111", _
                                                                "G000000000", "G000UU0162", "G000UU0162"}, _
                                           .items = New List(Of String)}
        s.items.AddRange(New String() {"G000IG0011", "G000IG0012", "G000IG0013", "G000IG0014", _
                                       "G000IG0015", "G000IG0016", "G000IG1001", "G000IG1002"})
        Dim expected As New RandStack.DesiredStats With {.Race = New List(Of Integer), _
                                                         .LootCost = 200 + 450 + 1500 + 200 + 450 + 1500 + 1000 + 3000, _
                                                         .StackSize = 6, _
                                                         .MeleeCount = 2, _
                                                         .MaxGiants = 1, _
                                                         .ExpStackKilled = 320 + 100 + 535 + 175 * 2, _
                                                         .ExpBarAverage = 2100 + 725 + 2800 + 1100 * 2}
        expected.ExpBarAverage /= expected.StackSize
        expected.Race.AddRange(New Integer() {1, 2, 3, 4})
        Dim ok As Boolean = True
        Dim calculated As RandStack.DesiredStats = target.StackStats(s)
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
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, False)
        Dim ok As Boolean = True
        Dim cost, sum As Integer
        Dim genitems As List(Of String)
        For i As Integer = 0 To 100 Step 1
            cost = target.minItemGoldCost + i * 125
            genitems = target.ItemsGen(cost)
            sum = 0
            For Each id As String In genitems
                If target.itemType.Item(target.FindItemStats(id).Type) = "JEWEL" Then
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
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, False)

        Dim ok As Boolean = True

        Dim stats As New RandStack.DesiredStats With {.ExpBarAverage = 1450, .ExpStackKilled = 1000, .Race = New List(Of Integer), _
                                                      .StackSize = 3, .MaxGiants = 1, .MeleeCount = 3, .LootCost = 1200}
        Dim races() As Integer = New Integer() {1, 2}
        Dim raceokL(UBound(races)), raceokF(UBound(races)) As Boolean
        stats.Race.AddRange(races)

        For i As Integer = 0 To 10000 Step 1
            Dim stack As RandStack.Stack = target.Gen(stats, True)
            ok = TestStack(stack, target, races, raceokL, raceokF)
            If Not ok Then Exit For
        Next i
        For r As Integer = 0 To UBound(races) Step 1
            If Not raceokL(r) Or Not raceokF(r) Then ok = False
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
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, ItemsList, excludeList, False)

        Dim ok As Boolean = True
        Dim UnapropriateStacks() As RandStack.Stack = New RandStack.Stack() { _
            New RandStack.Stack With {.leaderPos = -1, .pos = Nothing}, _
            New RandStack.Stack With {.leaderPos = -1, .pos = New String() {"G000UU0001", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 0, .pos = New String() {"G000UU0001", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 1, .pos = New String() {"G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 0, .pos = New String() {"G000UU5201", "G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 1, .pos = New String() {"G000UU5201", "G000UU0006", "G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 0, .pos = New String() {"G000UU5230", "G000UU0006", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 0, .pos = New String() {"G000UU0001", "G000UU0006", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 1, .pos = New String() {"G000UU0000", "G000UU5230", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}, _
            New RandStack.Stack With {.leaderPos = 1, .pos = New String() {"G000UU0000", "G000UU5201", "G000UU0000", "G000UU0000", "G000UU0000", "G000UU0000"}}}

        For Each item As RandStack.Stack In UnapropriateStacks
            If TestStack(item, target) Then
                ok = False
                Exit For
            End If
        Next item

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
