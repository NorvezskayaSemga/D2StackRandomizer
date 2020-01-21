Public Class SetName

    Private link As String = "https://norvezskayasemga.wixsite.com/d2bfwmod/donaters"
    Private path As String = ".\Donaters.txt"
    Private name() As String = Nothing
    Private weight() As Double = Nothing
    Private comm As New Common
    Private rndgen As New RndValueGen
    Private exclude As New List(Of String)
    Private excludeRace As New List(Of Integer)
    Private maxLen As Integer = 30

    Public Sub New()
        Dim t As Date = ReadNames()
        Dim download As Boolean = False
        If IsNothing(name) Then
            download = True
        Else
            Dim d As Integer = 365 * (DateAndTime.Today.Year - t.Year) + t.DayOfYear - DateAndTime.Today.DayOfYear
            If d > 7 Then download = True
        End If
        If download Then
            Call DownloadList()
            Call PrintNames()
        End If
        Call ReadExclusions()
    End Sub

    Private Sub DownloadList()
        Dim w As New System.Net.WebClient
        Try
            Dim str() As String = w.DownloadString(link).Replace(Chr(10), vbNewLine).Split(CChar(vbNewLine))
            Dim base As String = ""
            For Each s As String In str
                If s.Contains("var warmupData") Then
                    base = s
                    Exit For
                End If
            Next s
            If Not base = "" Then
                str = base.Split(CChar("<"))
                Dim n As Integer = 0
                Dim m As Integer = 0
                For i As Integer = 0 To UBound(str) Step 1
                    If Not str(i).Contains("font-family") Then
                        str(i) = ""
                    Else
                        str(i) = str(i).Substring(str(i).IndexOf(">") + 1)
                        If n >= m Then
                            m += 1
                        Else
                            n += 1
                        End If
                    End If
                Next i
                If n > 0 Then
                    ReDim name(n - 1)
                    n = 0
                    m = 0
                    For i As Integer = 0 To UBound(str) Step 1
                        If Not str(i) = "" Then
                            If n = m Then
                                name(n) = str(i)
                            Else
                                weight(n) = CDbl(str(i))
                            End If
                            If n >= m Then
                                m += 1
                            Else
                                n += 1
                            End If
                        End If
                    Next i
                    Dim wsum As Double = 0
                    For i As Integer = 0 To UBound(name) Step 1
                        wsum += weight(i)
                    Next i
                    For i As Integer = 0 To UBound(name) Step 1
                        weight(i) /= wsum
                    Next i
                End If
                base = base
            End If
        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try
    End Sub

    Private Sub PrintNames()
        If IsNothing(name) Then Exit Sub
        Dim str As String = ""
        For i As Integer = 0 To UBound(name) Step 1
            str &= name(i) & vbTab & weight(i) & vbNewLine
        Next i
        IO.File.WriteAllText(path, str)
    End Sub
    Private Function ReadNames() As Date
        If Not IO.File.Exists(path) Then Return Nothing
        Dim str() As String = comm.TxtSplit(IO.File.ReadAllText(path))
        ReDim name(UBound(str)), weight(UBound(str))
        Dim s() As String
        For i As Integer = 0 To UBound(name) Step 1
            s = str(i).Split(CChar(" "))
            For j As Integer = 0 To UBound(s) - 1 Step 1
                name(i) &= s(j)
                If j < UBound(s) - 1 Then name(i) &= " "
            Next j
            weight(i) = CDbl(s(UBound(s)))
        Next i
        Return IO.File.GetCreationTime(path)
    End Function
    Private Sub ReadExclusions()
        excludeRace.Add(comm.RaceIdentifierToSubrace("Greenskins"))
        Dim str() As String = comm.TxtSplit(My.Resources.ExcludeIDsForNames)
        For i As Integer = 0 To UBound(str) Step 1
            exclude.Add(str(i).ToUpper)
        Next i
    End Sub

    ''' <param name="stacks">Уже сгенерированные стэки</param>
    ''' <param name="R">Инициализированный класс</param>
    Public Sub GenNames(ByRef stacks() As RandStack.Stack, ByRef R As RandStack)
        If IsNothing(stacks) Or IsNothing(R) Then Exit Sub
        Dim IDs As New List(Of Integer)
        If Not IsNothing(name) Then
            For i As Integer = 0 To UBound(name) Step 1
                IDs.Add(i)
            Next i
        End If
        For i As Integer = 0 To UBound(stacks) Step 1
            If Not IsNothing(stacks(i)) Then
                If stacks(i).leaderPos > -1 AndAlso Not IsNothing(stacks(i).pos) Then
                    stacks(i).name = SetName(stacks(i).pos(stacks(i).leaderPos).ToUpper, R, IDs)
                Else
                    stacks(i).name = ""
                End If
            End If
        Next i
    End Sub
    Private Function SetName(ByRef leaderID As String, ByRef R As RandStack, ByRef IDs As List(Of Integer)) As String
        Dim u As RandStack.Unit = R.FindUnitStats(leaderID)
        Dim res As String = u.name
        If excludeRace.Contains(u.race) Then Return res
        If exclude.Contains(u.unitID) Then Return res
        If IDs.Count > 0 AndAlso rndgen.PRand(0, 1) > 0.9 Then
            Dim i As Integer = comm.RandomSelection(IDs, weight, True)
            IDs.Remove(i)
            res &= " " & name(i)
            If res.Length > maxLen Then res = res.Substring(0, maxLen)
        End If
        Return res
    End Function
End Class
