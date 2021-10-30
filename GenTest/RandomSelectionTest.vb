Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for RandomSelectionTest and is intended
'''to contain all RandomSelectionTest Unit Tests
'''</summary>
<TestClass()> _
Public Class RandomSelectionTest


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
    '''A test for Item
    '''</summary>
    <TestMethod()> _
    Public Sub ItemTest()
        Dim target As RandomSelection_Accessor = New RandomSelection_Accessor(100, Nothing)
        For i As Integer = 0 To target.upperBound Step 1
            For k As Integer = 1 To i Step 1
                target.Add(i)
            Next k
        Next i
        Dim ok As Boolean = True
        For i As Integer = 0 To target.Count - 1 Step 1
            If Not target.Item(i) = Item(target, i) Then
                ok = False
            End If
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
    Private Function Item(ByRef target As RandomSelection_Accessor, ByVal n As Integer) As Integer
        Dim k As Integer
        For i As Integer = 0 To target.upperBound Step 1
            If target.Contains(i) Then
                For j As Integer = 1 To target.Checked(i) Step 1
                    If k = n Then
                        Return i
                    End If
                    k += 1
                Next j
            End If
        Next i
        Throw New Exception("Unexpected input: " & n)
    End Function

    '''<summary>
    '''A test for GetNext
    '''</summary>
    <TestMethod()> _
    Public Sub GetNextTest()
        Dim ok As Boolean = True
        Dim expected(1000000) As Integer
        For p As Integer = 0 To 2 Step 1
            Dim n As Integer = -1
            Dim i As Integer = -1 + p * 10
            Do While True
                i += 1
                For j As Integer = 1 To i Step 1
                    If n = UBound(expected) Then Exit Do
                    n += 1
                    expected(n) = i
                Next j
            Loop
            Dim target As RandomSelection = New RandomSelection(expected.Max + 1, Nothing)
            For j As Integer = 0 To UBound(expected) Step 1
                target.Add(expected(j))
            Next j
            n = -1
            For Each actual As Integer In target
                n += 1
                If Not actual = expected(n) Then
                    ok = False
                    Exit For
                End If
            Next actual
        Next p
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
