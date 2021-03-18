Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports ImpenetrableMeshShow



'''<summary>
'''This is a test class for D2ScriptsTest and is intended
'''to contain all D2ScriptsTest Unit Tests
'''</summary>
<TestClass()> _
Public Class D2ScriptsTest


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
    '''A test for LevelToExpSlow
    '''</summary>
    <TestMethod()> _
    Public Sub LevelToExpTest()
        Dim baseLevel As Integer = 3
        Dim dynLevel As Integer = 10
        Dim baseExp As Integer = 2150
        Dim expNext1() As Integer = {250, 0, 250, 0}
        Dim expNext2() As Integer = {100, 100, 0, 0}
        Dim expected(3)() As Integer
        expected(0) = {0, 0, 0, 0, 2150, 4550, 7200, 10100, 13250, 16650, 20300, 24200, 28200, 32300, 36500, 40800}
        expected(1) = {0, 0, 0, 0, 2150, 4300, 6450, 8600, 10750, 12900, 15050, 17200, 19450, 21800, 24250, 26800}
        expected(2) = {0, 0, 0, 0, 2150, 4550, 7200, 10100, 13250, 16650, 20300, 24200, 28100, 32000, 35900, 39800}
        expected(3) = {0, 0, 0, 0, 2150, 4300, 6450, 8600, 10750, 12900, 15050, 17200, 19350, 21500, 23650, 25800}

        Dim actual1, actual2, exp As Integer
        Dim ok As Boolean = True

        For p As Integer = 0 To UBound(expected) Step 1
            For currentLevel As Integer = 0 To UBound(expected(p)) Step 1
                actual1 = D2Scripts.LevelToExpFast(baseLevel, currentLevel, dynLevel, baseExp, expNext1(p), expNext2(p))
                actual2 = D2Scripts.LevelToExpSlow(baseLevel, currentLevel, dynLevel, baseExp, expNext1(p), expNext2(p))
                exp = expected(p)(currentLevel)
                If Not exp = actual1 Then
                    ok = False
                    Exit For
                ElseIf Not exp = actual2 Then
                    ok = False
                    Exit For
                End If
            Next currentLevel
        Next p
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for ExpToLevelSlow
    '''</summary>
    <TestMethod()> _
    Public Sub ExpToLevelTest()
        Dim baseLevel As Integer = 3
        Dim dynLevel As Integer = 10
        Dim baseExp As Integer = 2150
        Dim expNext1() As Integer = {250, 0, 250, 0}
        Dim expNext2() As Integer = {100, 100, 0, 0}

        Dim ok As Boolean = True
        Dim currentExp As Integer
        Dim actual As Integer

        For p As Integer = 0 To UBound(expNext1) Step 1
            For currentLevel As Integer = baseLevel To 20 Step 1
                For add As Integer = 0 To 5 Step 1
                    Dim addExp As Integer = baseExp * CDbl(add / 6)
                    currentExp = D2Scripts.LevelToExpFast(baseLevel, currentLevel, dynLevel, baseExp, expNext1(p), expNext2(p))
                    currentExp += addExp
                    actual = D2Scripts.ExpToLevelSlow(currentExp, baseLevel, dynLevel, baseExp, expNext1(p), expNext2(p))
                    If Not currentLevel = actual Then
                        ok = False
                        Exit For
                    End If
                Next add
            Next currentLevel
        Next p

        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
