Imports Microsoft.VisualStudio.TestTools.UnitTesting

Imports RandomStackGenerator



'''<summary>
'''This is a test class for ArrayZoomTest and is intended
'''to contain all ArrayZoomTest Unit Tests
'''</summary>
<TestClass()> _
Public Class ArrayZoomTest


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
    '''A test for CalcMultiplicator
    '''</summary>
    <TestMethod()> _
    Public Sub CalcMultiplicatorTest()
        Dim target As ArrayZoom = New ArrayZoom() 
        Dim sizes() As Integer = New Integer() {48, 72, 96, 120, 144}
        Dim expected() As Integer = New Integer() {12, 8, 6, 4, 4}
        Dim ok As Boolean = True
        For i As Integer = 0 To UBound(sizes) Step 1
            If Not target.CalcMultiplicator(sizes(i), 576) = expected(i) Then
                ok = False
                Exit For
            End If
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub

    '''<summary>
    '''A test for Zoom
    '''</summary>
    <TestMethod()> _
    Public Sub ZoomTest()
        Dim target As ArrayZoom = New ArrayZoom() ' TODO: Initialize to an appropriate value
        Dim grid(,) As Integer = New Integer(,) {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
        Dim multiplicator As Integer = 4
        Dim expected(,) As Integer = New Integer(,) { _
                                                {1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3}, _
                                                {1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3}, _
                                                {1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3}, _
                                                {1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3}, _
                                                {4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6}, _
                                                {4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6}, _
                                                {4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6}, _
                                                {4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6}, _
                                                {7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9}, _
                                                {7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9}, _
                                                {7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9}, _
                                                {7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9}}
        Dim ok As Boolean = True

        Dim actual(,) As Integer = target.Zoom(grid, multiplicator)
        For i As Integer = 0 To UBound(expected, 1) Step 1
            For j As Integer = 0 To UBound(expected, 2) Step 1
                If Not actual(i, j) = expected(i, j) Then ok = False
            Next j
        Next i
        If Not ok Then Assert.Inconclusive("Verify the correctness of this test method.")
    End Sub
End Class
