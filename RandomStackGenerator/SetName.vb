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
    Private IDs As New List(Of Integer)

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
        Dim Tn() As String = Nothing
        Dim Tw() As Double = Nothing
        Try
            Dim byteData() As Byte = w.DownloadData(link)
            Dim d As String = Text.Encoding.UTF8.GetString(byteData)
            Dim str() As String = d.Replace(Chr(10), vbNewLine).Split(CChar(vbNewLine))
            Dim base As String = ""
            For Each s As String In str
                If s.Contains("var warmupData") Then
                    base = s
                    Exit For
                End If
            Next s
            If base = "" Then
                w.Dispose()
                Exit Sub
            End If
            str = base.Split(CChar("<"))
            Dim n As Integer = 0
            Dim m As Integer = 0
            For i As Integer = 0 To UBound(str) Step 1
                If Not str(i).Contains("font-family") Then
                    str(i) = ""
                Else
                    str(i) = str(i).Substring(str(i).IndexOf(">") + 1)
                    Console.WriteLine(str(i))
                    If n >= m Then
                        m += 1
                    Else
                        n += 1
                    End If
                End If
            Next i
            If n > 0 Then
                ReDim Tn(n - 1), Tw(n - 1)
                n = 0
                m = 0
                For i As Integer = 0 To UBound(str) Step 1
                    If Not str(i) = "" Then
                        If n = m Then
                            Tn(n) = str(i)
                        Else
                            Tw(n) = CDbl(str(i))
                        End If
                        If n >= m Then
                            m += 1
                        Else
                            n += 1
                        End If
                    End If
                Next i
                Dim wsum As Double = 0
                For i As Integer = 0 To UBound(Tn) Step 1
                    wsum += Tw(i)
                Next i
                For i As Integer = 0 To UBound(Tn) Step 1
                    Tw(i) /= wsum
                Next i
            End If
        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try
        If Not IsNothing(Tn) And Not IsNothing(Tw) Then
            Dim added As New Dictionary(Of String, Integer)
            ReDim name(UBound(Tn)), weight(UBound(Tw))
            For i As Integer = 0 To UBound(Tn) Step 1
                Dim s As String = Tn(i).ToUpper
                If Not added.ContainsKey(s) Then
                    name(added.Count) = Tn(i)
                    added.Add(s, added.Count)
                End If
                weight(added.Item(s)) += Tw(i)
            Next i
            ReDim Preserve name(added.Count - 1), weight(added.Count - 1)
        End If
        w.Dispose()
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


    '''<summary>Присвоит имена всем отрядам в списке</summary>
    ''' <param name="stack">Уже сгенерированные стэки</param>
    ''' <param name="R">Инициализированный класс</param>
    ''' <param name="newMapGen">True, если это первый вызов этой функции при генерации новой карты</param>
    Public Sub GenName(ByRef stack As AllDataStructues.Stack, ByRef R As RandStack, ByVal newMapGen As Boolean)
        If IsNothing(stack) Or IsNothing(R) Then Exit Sub
        If Not IsNothing(name) And newMapGen Then
            IDs.Clear()
            For i As Integer = 0 To UBound(name) Step 1
                IDs.Add(i)
            Next i
        End If
        If stack.leaderPos > -1 AndAlso Not IsNothing(stack.pos) Then
            stack.name = SetName(stack.pos(stack.leaderPos).ToUpper, R)
        Else
            stack.name = ""
        End If
    End Sub
    Private Function SetName(ByRef leaderID As String, ByRef R As RandStack) As String
        Dim u As AllDataStructues.Unit = R.FindUnitStats(leaderID)
        Dim res As String = u.name
        If excludeRace.Contains(u.race) Then Return res '& " race_excluded"
        If exclude.Contains(u.unitID) Then Return res '& " unit_excluded"
        If IDs.Count > 0 AndAlso rndgen.PRand(0, 1) > 0.9 Then
            Dim i As Integer = comm.RandomSelection(IDs, weight, True)
            IDs.Remove(i)
            res &= " " & name(i)
            If res.Length > maxLen Then res = res.Substring(0, maxLen)
            'Else
            '    If IDs.Count = 0 Then
            '        Return res & " no_names"
            '    Else
            '        Return res & " random_value<0.9"
            '    End If
        End If
        Return res
    End Function
End Class
