Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for RndValueGenTest and is intended
'''to contain all RndValueGenTest Unit Tests
'''</summary>
<TestClass()> _
Public Class RndValueGenTest


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
    '''A test for RndIntFast
    '''</summary>
    <TestMethod()> _
    Public Sub RndIntFastTest1()
        Dim size As Integer = 1000
        Dim m As Integer = 123
        Dim generated() As Boolean
        Dim ok As Boolean = True
        For min As Integer = 0 To m Step m
            For seed As Integer = 0 To 1 Step 1
                ReDim generated(size)
                Dim target As RndValueGen = New RndValueGen(-seed + (1 - seed) * Integer.MaxValue)
                For i As Integer = 0 To 20 * size Step 1
                    generated(target.RndIntFast(min, size + min) - min) = True
                Next i
                For i As Integer = 0 To size Step 1
                    If Not generated(i) Then ok = False
                Next i
            Next seed
            If Not ok Then Exit For
        Next min
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for RndIntFast
    '''</summary>
    <TestMethod()> _
    Public Sub RndIntFastTest2()
        Dim target As RndValueGen = New RndValueGen()
        Dim ok As Boolean = True
        For min As Integer = 0 To 100 Step 1
            For max As Integer = min To 100 Step 1
                Dim r As Integer = target.RndIntFast(min, max)
                If r < min Then ok = False
                If r > max Then ok = False
            Next max
            If Not ok Then Exit For
        Next min
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for RndInt
    '''</summary>
    <TestMethod()> _
    Public Sub RndIntTest()
        Dim size As Integer = 1000
        Dim m As Integer = 123
        Dim generated() As Boolean
        Dim ok As Boolean = True
        For min As Integer = 0 To m Step m
            ReDim generated(size)
            Dim target As RndValueGen = New RndValueGen()
            For i As Integer = 0 To 50 * size Step 1
                generated(target.RndInt(min, size + min, True) - min) = True
            Next i
            For i As Integer = 0 To size Step 1
                If Not generated(i) Then ok = False
            Next i
        Next min
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Shuffle
    '''</summary>
    <TestMethod()> _
    Public Sub ShuffleTest()
        Dim target As RndValueGen = New RndValueGen()
        Dim n() As Integer
        For i As Integer = -1 To 100 Step 1
            ReDim n(i)
            Call target.Shuffle(n)
        Next i
    End Sub

End Class
