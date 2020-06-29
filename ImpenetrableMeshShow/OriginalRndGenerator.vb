Public Class OriginalRndGenerator

    Dim data(250) As UInteger

    Dim randomSeed As UInteger

    Public Sub New()
        randomSeed = 0
        Dim b() As Byte = IO.File.ReadAllBytes(".\Resources\randGeneratorData.binary")
        For i As Integer = 0 To UBound(data) Step 1
            data(i) = BitConverter.ToInt32(b, i * 4)
        Next i
    End Sub

    Private Function sub_67993C() As UInteger

        Dim v0, v2, result As UInteger

        v0 = randomSeed
        v2 = CalcV2()

        If randomSeed < 147 Then v2 = randomSeed + 103

        Call CalcData(v2)
        result = data(v0)

        If v0 < 249 Then
            randomSeed = v0 + 1
        Else
            randomSeed = 0
        End If

        Return result
    End Function

    Private Sub CalcData(ByRef v2 As UInteger)
        Dim v As ULong = data(randomSeed)
        Dim degree As UInteger = data(v2) Mod 8
        Dim out As ULong = 1

        For i As UInteger = 0 To degree Step 1
            out *= v
            If out > UInteger.MaxValue Then
                out = (out Mod (UInteger.MaxValue - 4)) + 2
            End If
        Next i
        data(randomSeed) = out
    End Sub
    Private Function CalcV2() As UInteger
        If randomSeed >= 147 Then
            Return randomSeed - 147
        Else
            Return 104 + randomSeed
        End If
    End Function

    '// Generates random value in range [0 : maxValue)
    Public Function randomNumberUpTo(maxValue As UInteger) As UInteger
        Dim v1, v2, v3 As UInteger

        Do
            Call sub_67993C()
            v1 = randomSeed
            v2 = CalcV2()

            If randomSeed < 147 Then v2 = randomSeed + 103

            Call CalcData(v2)
            v3 = data(v1)
            If v1 < 249 Then
                randomSeed = v1 + 1
            Else
                randomSeed = 0
            End If
        Loop While (v3 >= maxValue * (UShort.MaxValue / maxValue))

        Return v3 Mod maxValue
    End Function


End Class
