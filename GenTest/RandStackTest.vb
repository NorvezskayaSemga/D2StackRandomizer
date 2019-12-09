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

    Dim UnitsList() As RandStack.Unit = Nothing
    Private Sub ReadTestUnits()
        Dim s() As String = RandomStackGenerator.My.Resources.TestUnitsTable.Split(vbNewLine)
        Dim r() As String
        ReDim UnitsList(UBound(s) - 1)
        For i As Integer = 1 To UBound(s) Step 1
            r = s(i).Split(vbTab)
            If r.Length = 12 Then
                Do While Not r(0).Substring(0, 1).ToLower = "g" And r(0).Length > 1
                    r(0) = r(0).Substring(1)
                Loop
                UnitsList(i - 1).unitID = r(0)
                If r(1) = 8 Then
                    UnitsList(i - 1).unitBranch = r(1)
                Else
                    UnitsList(i - 1).unitBranch = r(4)
                End If
                UnitsList(i - 1).level = r(2)
                UnitsList(i - 1).race = r(3)
                UnitsList(i - 1).small = r(5)
                UnitsList(i - 1).EXPkilled = r(7)
                UnitsList(i - 1).EXPnext = r(8)
                UnitsList(i - 1).leadership = r(9)
                UnitsList(i - 1).waterOnly = r(10)
                UnitsList(i - 1).reach = r(11)
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
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, False)

        Dim serial_ok, parallel_ok As Boolean
        serial_ok = True
        parallel_ok = True
        For n As Integer = 1 To 10 Step 1
            Dim ok As Boolean
            Dim r As Integer
            For j As Integer = 1 To n Step 1
                ok = False
                For attempt = 0 To 100 * n Step 1
                    r = target.RndPos(n, True)
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
                     r = target.RndPos(n, False)
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
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, False)
        Dim maxval As Integer = 10000
        Dim ok, g(maxval / 10), tmpok As Boolean
        ok = True
        For j As Integer = 0 To 10 Step 1
            For i As Integer = 0 To maxval * 10 Step 1
                g(target.Rand(0, CDbl(maxval), True) / 10) = True
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
                 g(target.Rand(0, CDbl(maxval), False) / 10) = True
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
    Public Sub GenTest()
        If IsNothing(UnitsList) Then Call ReadTestUnits()
        Dim target As RandStack_Accessor = New RandStack_Accessor(UnitsList, False)

        Dim busytransfer() As Integer = New Integer() {1, -1, 3, -1, 5, -1}
        Dim firstrow() As Integer = New Integer() {0, 2, 4}
        Dim secondrow() As Integer = New Integer() {1, 3, 5}
        Dim ok As Boolean = True

        Dim stats As New RandStack.DesiredStats
        Dim unit As RandStack.Unit
        Dim races() As String = New String() {"H", "U", "L", "C", "E", "N", "G", "D", "S", "W", "B", "A"}
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
                                stats.Race = target.comm.RaceIdentifierToSubrace(races(r))

                                For g As Integer = 0 To 1 Step 1
                                    If g = 0 Then
                                        GroundTile = False
                                        If races(r) = "W" Then g = 1
                                    Else
                                        GroundTile = True
                                    End If

                                    Dim stack As RandStack.Stack = target.Gen(stats, GroundTile)

                                    If stack.leaderPos < 0 Or IsNothing(stack.pos) Then
                                        ok = False
                                        GoTo exittest
                                    End If
                                    For i As Integer = 0 To UBound(stack.pos) Step 1
                                        If stack.pos(i) = "" Then
                                            ok = False
                                            GoTo exittest
                                        ElseIf Not stack.pos(i) = "G000000000" Then
                                            unit = target.FindUnitStats(stack.pos(i))
                                            If (unit.unitBranch = 5 And Not i = stack.leaderPos) Or (Not unit.unitBranch = 5 And i = stack.leaderPos) Then
                                                ok = False
                                                GoTo exittest
                                            End If
                                            If Not unit.small Then
                                                If busytransfer(i) = -1 Or Not stack.pos(busytransfer(i)) = "G000000000" Then
                                                    ok = False
                                                    GoTo exittest
                                                End If
                                            End If
                                            If unit.reach = 3 Then
                                                For j As Integer = 0 To UBound(secondrow) Step 1
                                                    If i = secondrow(j) Then
                                                        ok = False
                                                        GoTo exittest
                                                    End If
                                                Next j
                                            End If
                                        End If
                                    Next i
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
End Class
