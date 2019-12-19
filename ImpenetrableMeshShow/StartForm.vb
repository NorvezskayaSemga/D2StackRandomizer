Imports RandomStackGenerator

Public Class StartForm

    Dim genmesh As New InpenetrableMeshGen
    Dim zoom As New ArrayZoom
    Dim draw As New ColorSelector

    Private Sub GenButton_Click() Handles GenButton.Click


        Dim grid(,) As Integer
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

        grid = genmesh.Gen(48, 48, races)

        Dim mult As Integer = zoom.CalcMultiplicator(UBound(grid) + 1)
        Dim mgrid(,) As Integer = zoom.Zoom(grid, mult)
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
