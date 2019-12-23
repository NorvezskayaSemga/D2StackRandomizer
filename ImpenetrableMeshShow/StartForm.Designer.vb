<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class StartForm
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.GenButton = New System.Windows.Forms.Button()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.sel3 = New System.Windows.Forms.RadioButton()
        Me.sel2 = New System.Windows.Forms.RadioButton()
        Me.sel1 = New System.Windows.Forms.RadioButton()
        Me.SymmCheckBox = New System.Windows.Forms.CheckBox()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'PictureBox1
        '
        Me.PictureBox1.Location = New System.Drawing.Point(12, 12)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(576, 576)
        Me.PictureBox1.TabIndex = 0
        Me.PictureBox1.TabStop = False
        '
        'GenButton
        '
        Me.GenButton.Location = New System.Drawing.Point(611, 12)
        Me.GenButton.Name = "GenButton"
        Me.GenButton.Size = New System.Drawing.Size(124, 46)
        Me.GenButton.TabIndex = 1
        Me.GenButton.Text = "Generate"
        Me.GenButton.UseVisualStyleBackColor = True
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.sel3)
        Me.Panel1.Controls.Add(Me.sel2)
        Me.Panel1.Controls.Add(Me.sel1)
        Me.Panel1.Location = New System.Drawing.Point(612, 74)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(122, 94)
        Me.Panel1.TabIndex = 2
        '
        'sel3
        '
        Me.sel3.AutoSize = True
        Me.sel3.Location = New System.Drawing.Point(18, 59)
        Me.sel3.Name = "sel3"
        Me.sel3.Size = New System.Drawing.Size(60, 17)
        Me.sel3.TabIndex = 2
        Me.sel3.Text = "4 races"
        Me.sel3.UseVisualStyleBackColor = True
        '
        'sel2
        '
        Me.sel2.AutoSize = True
        Me.sel2.Location = New System.Drawing.Point(18, 36)
        Me.sel2.Name = "sel2"
        Me.sel2.Size = New System.Drawing.Size(60, 17)
        Me.sel2.TabIndex = 1
        Me.sel2.Text = "3 races"
        Me.sel2.UseVisualStyleBackColor = True
        '
        'sel1
        '
        Me.sel1.AutoSize = True
        Me.sel1.Checked = True
        Me.sel1.Location = New System.Drawing.Point(18, 13)
        Me.sel1.Name = "sel1"
        Me.sel1.Size = New System.Drawing.Size(60, 17)
        Me.sel1.TabIndex = 0
        Me.sel1.TabStop = True
        Me.sel1.Text = "2 races"
        Me.sel1.UseVisualStyleBackColor = True
        '
        'SymmCheckBox
        '
        Me.SymmCheckBox.AutoSize = True
        Me.SymmCheckBox.Location = New System.Drawing.Point(630, 188)
        Me.SymmCheckBox.Name = "SymmCheckBox"
        Me.SymmCheckBox.Size = New System.Drawing.Size(98, 17)
        Me.SymmCheckBox.TabIndex = 3
        Me.SymmCheckBox.Text = "Apply symmetry"
        Me.SymmCheckBox.UseVisualStyleBackColor = True
        '
        'StartForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(773, 606)
        Me.Controls.Add(Me.SymmCheckBox)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.GenButton)
        Me.Controls.Add(Me.PictureBox1)
        Me.Name = "StartForm"
        Me.Text = "Form1"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents GenButton As System.Windows.Forms.Button
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents sel3 As System.Windows.Forms.RadioButton
    Friend WithEvents sel2 As System.Windows.Forms.RadioButton
    Friend WithEvents sel1 As System.Windows.Forms.RadioButton
    Friend WithEvents SymmCheckBox As System.Windows.Forms.CheckBox

End Class
