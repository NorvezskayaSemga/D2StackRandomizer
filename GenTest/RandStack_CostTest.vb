Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for RandStack_CostTest and is intended
'''to contain all RandStack_CostTest Unit Tests
'''</summary>
<TestClass()> _
Public Class RandStack_CostTest


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

    Private testStrings() As String = New String() { _
                             "g1500:r0000:y0000:e0000:w0000", _
                             "g0000:r0000:y0100:e0000:w0000:b0000", _
                             "g0000:r0200:y0000:e0000:w0000:b0000", _
                             "g0000:r0000:y0000:e0000:w0300:b0000", _
                             "g0000:r0000:y0000:e0400:w0000:b0000", _
                             "g0000:r0000:y0000:e0000:w0000:b0500"}

    Private testCosts() As RandStack.Cost = New RandStack.Cost() {
                       New RandStack.Cost With {.Gold = 1500}, _
                       New RandStack.Cost With {.Blue = 100}, _
                       New RandStack.Cost With {.Red = 200}, _
                       New RandStack.Cost With {.White = 300}, _
                       New RandStack.Cost With {.Black = 400}, _
                       New RandStack.Cost With {.Green = 500}}

    '''<summary>
    '''A test for Print
    '''</summary>
    <TestMethod()> _
    Public Sub PrintTest()
        Dim ok As Boolean = True
        Dim s As String
        For i As Integer = 0 To UBound(testCosts) Step 1
            s = RandStack_Accessor.Cost.Print(testCosts(i))
            If Not s = testStrings(i) And (Not testStrings(i).Contains("b") _
                                           And Not s = testStrings(i) & ":b0000") Then
                ok = False
                Exit For
            End If
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Read
    '''</summary>
    <TestMethod()> _
    Public Sub ReadTest()
        Dim ok As Boolean = True
        Dim t As RandStack.Cost
        For i As Integer = 0 To UBound(testCosts) Step 1
            t = RandStack_Accessor.Cost.Read(testStrings(i))
            If Not t.Black = testCosts(i).Black _
            Or Not t.Blue = testCosts(i).Blue _
            Or Not t.Gold = testCosts(i).Gold _
            Or Not t.Green = testCosts(i).Green _
            Or Not t.Red = testCosts(i).Red _
            Or Not t.White = testCosts(i).White Then
                ok = False
                Exit For
            End If
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
