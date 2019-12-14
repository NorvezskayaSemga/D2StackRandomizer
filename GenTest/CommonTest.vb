Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for CommonTest and is intended
'''to contain all CommonTest Unit Tests
'''</summary>
<TestClass()> _
Public Class CommonTest


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
            testContextInstance = Value
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


    '''<summary>
    '''A test for ParseDesiredStackStatsFile
    '''</summary>
    <TestMethod()> _
    Public Sub ParseDesiredStackStatsFileTest1()
        Dim target As Common = New Common()
        Dim ok As Boolean = True
        Dim expected() As RandStack.DesiredStats = New RandStack.DesiredStats() {
            New RandStack.DesiredStats With {.LocationName = "location1", .ExpBarAverage = 1000, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 1, .MaxGiants = 0, .MeleeCount = 2, .LootCost = 0}, _
            New RandStack.DesiredStats With {.LocationName = "testloc2", .ExpBarAverage = 100, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 3, .MaxGiants = 1, .MeleeCount = 0, .LootCost = 3400}, _
            New RandStack.DesiredStats With {.LocationName = "loc3", .ExpBarAverage = 1000, .ExpStackKilled = 200, _
               .Race = New List(Of Integer), .StackSize = 1, .MaxGiants = 0, .MeleeCount = 2, .LootCost = 0}
            }
        expected(0).Race.AddRange(New Integer() {2})
        expected(1).Race.AddRange(New Integer() {2, 9})
        expected(2).Race.AddRange(New Integer() {1})

        Dim actual() As RandStack.DesiredStats = target.ParseDesiredStackStatsFile("%testfile%")

        If expected.Length = actual.Length Then
            For i As Integer = 0 To UBound(expected) Step 1
                If Not expected(i).ExpBarAverage = actual(i).ExpBarAverage Then ok = False
                If Not expected(i).ExpStackKilled = actual(i).ExpStackKilled Then ok = False
                If Not expected(i).LocationName = actual(i).LocationName Then ok = False
                If Not expected(i).LootCost = actual(i).LootCost Then ok = False
                If Not expected(i).MaxGiants = actual(i).MaxGiants Then ok = False
                If Not expected(i).MeleeCount = actual(i).MeleeCount Then ok = False
                If Not expected(i).StackSize = actual(i).StackSize Then ok = False
                If expected(i).Race.Count = actual(i).Race.Count Then
                    For Each item As Integer In expected(i).Race
                        If Not actual(i).Race.Contains(item) Then ok = False
                    Next item
                Else
                    ok = False
                End If
            Next i
        Else
            ok = False
        End If

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for ParseDesiredStackStatsFile
    '''</summary>
    <TestMethod()> _
    Public Sub ParseDesiredStackStatsFileTest2()
        Dim target As Common = New Common()
        Dim ok As Boolean = True
        Dim path As String = "%testfile%"
        Dim expected As String = _
        "ID location1 AverageExpBar 1000 ExpStackKilled 200 Race U StackSize 1 MaxGiants 0 MeleeCount 2 LootCost 0" & vbNewLine & _
        "ID testloc2 AverageExpBar 100 ExpStackKilled 200 Race U+D StackSize 3 MaxGiants 1 MeleeCount 0 LootCost 3400" & vbNewLine & _
        "ID loc3 AverageExpBar 1000 ExpStackKilled 200 Race H StackSize 1 MaxGiants 0 MeleeCount 2 LootCost 0" & vbNewLine
        Dim content() As RandStack.DesiredStats = target.ParseDesiredStackStatsFile(path)
        Call target.WriteDesiredStackStats(path, content)

        If Not path = expected Then ok = False

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
