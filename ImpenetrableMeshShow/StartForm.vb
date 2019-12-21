Imports RandomStackGenerator

Public Class StartForm

    Dim genmesh As New InpenetrableMeshGen
    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector

    Private Sub GenButton_Click() Handles GenButton.Click


        Dim grid As InpenetrableMeshGen.Map
        '                 grid = New Integer(,) { _
        '                                        {1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3}, _
        '                                        {1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3}, _
        '                                        {4, 1, 1, 1, 2, 2, 2, 3, 3, 3, 6, 3}, _
        '                                        {4, 1, 1, 1, 2, 2, 2, 2, 3, 3, 6, 6}, _
        '                                        {4, 1, 1, 4, 5, 5, 5, 5, 6, 6, 6, 6}, _
        '                                        {4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6}, _
        '                                        {4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6}, _
        '                                        {7, 4, 4, 5, 5, 5, 5, 5, 6, 9, 6, 6}, _
        '                                        {7, 7, 5, 5, 5, 5, 8, 8, 9, 9, 9, 6}, _
        '                                        {7, 7, 7, 5, 5, 8, 8, 8, 9, 9, 9, 9}, _
        '                                        {7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9}, _
        '                                        {7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9}}

        Dim races As Integer
        If sel1.Checked Then
            races = 2
        ElseIf sel2.Checked Then
            races = 3
        Else
            races = 4
        End If

        grid = genmesh.Gen(96 - 1, 96 - 1, races, 20, 15, 0.3, 0.4)

        Dim mult As Integer = zoom.CalcMultiplicator(Math.Max(grid.xSize, grid.ySize) + 1)

        Dim t(grid.xSize, grid.ySize) As Integer
        For x As Integer = 0 To grid.xSize Step 1
            For y As Integer = 0 To grid.ySize Step 1
                If grid.board(x, y).locID.Count = 0 Or Not grid.board(x, y).isBorder Then
                    t(x, y) = 0
                Else
                    t(x, y) = grid.board(x, y).locID.Item(0)
                End If
            Next y
        Next x

        Dim mgrid(,) As Integer = zoom.Zoom(t, mult)
        Dim c(,) As Color = draw.MakeIslandsColorMap(mgrid)
        Dim xsize As Integer = UBound(c, 1)
        Dim ysize As Integer = UBound(c, 2)
        Dim img As Bitmap
        If IsNothing(Me.PictureBox1.Image) OrElse PictureBox1.Image.Width <> xsize + 1 OrElse PictureBox1.Image.Height <> ysize + 1 Then
            img = New Bitmap(xsize + 1, ysize + 1)
        Else
            img = PictureBox1.Image
        End If
        For i As Integer = 0 To xsize Step 1
            For j As Integer = 0 To ysize Step 1
                img.SetPixel(i, j, c(i, j))
            Next j
        Next i
        PictureBox1.Image = img
    End Sub
End Class
