Public Class SetName

    Private link As String = "https://norvezskayasemga.wixsite.com/d2bfwmod/donaters"
    Private path As String = ".\Donaters.txt"
    Private name() As String = Nothing
    Private weight() As Double = Nothing
    Private comm As New Common
    Private rndgen As New RndValueGen
    Private exclude As New List(Of String)
    Private excludeRace As New List(Of Integer)
    Private StackNameMaxLen As Integer = 30
    Private LordNameMaxLen As Integer = 15
    Private commonIDs, lordIDs As New List(Of Integer)
    Private defailtLords As New Dictionary(Of Integer, List(Of Integer))
    Private LordMinWeight As Double
    Private defaultLordNames(14)() As String
    Private lordDonationThreshold As Double = 999

    ''' <summary>Сюда генератор пишет лог</summary>
    Public log As Log

    Public Sub New()

        log = New Log(comm)

        defaultLordNames(1) = New String() {"Алексис", "Лотай", "Келли", "Моар"}
        defaultLordNames(2) = New String() {"Заориш", "Сагот", "Абрааль", "Интар"}
        defaultLordNames(3) = New String() {"Дхагот", "Ишангхти", "Абрааль", "Сагорат"}
        defaultLordNames(4) = New String() {"Магнерик", "Дагарик", "Атаульф", "Бродульф"}
        defaultLordNames(14) = New String() {"Лемваер", "Име'ель", "Рха'ане", "Гиндель"}

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
        Dim w As New System.Net.WebClient With {.Proxy = Nothing}
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
                            Tw(n) = ValueConverter.StrToDbl(str(i))
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
                LordMinWeight = lordDonationThreshold / wsum
            End If
        Catch ex As Exception
            AddToLog(-1, "Names downloader: " & ex.Message)
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
        Dim str As String = LordMinWeight.ToString
        For i As Integer = 0 To UBound(name) Step 1
            str &= vbNewLine & name(i) & vbTab & weight(i)
        Next i
        AddToLog(-1, "Writing " & path)
        Try
            IO.File.WriteAllText(path, str)
        Catch ex As Exception
            AddToLog(-1, ex.Message)
        End Try
        AddToLog(-1, "End writing")
    End Sub
    Private Function ReadNames() As Date
        If Not IO.File.Exists(path) Then Return Nothing
        Dim t As Date = Nothing
        AddToLog(-1, "Reading " & path)
        Try
            Dim alltext As String = IO.File.ReadAllText(path)
            AddToLog(-1, "Content:" & vbNewLine & alltext & vbNewLine & "-------------------")
            Dim str() As String = comm.TxtSplit(alltext)
            Dim s(), w As String
            Dim i0 As Integer
            If IsNumeric(str(0)) Then
                AddToLog(-1, "Lord min weight found")
                i0 = 1
            Else
                AddToLog(-1, "Lord min weight not found (don't worry)")
                i0 = 0
            End If
            ReDim name(UBound(str) - i0), weight(UBound(str) - i0)
            For i As Integer = i0 To UBound(name) Step 1
                s = str(i).Split(CChar(" "))
                For j As Integer = 0 To UBound(s) - 1 Step 1
                    name(i - i0) &= s(j)
                    If j < UBound(s) - 1 Then name(i) &= " "
                Next j
                w = s(UBound(s))
                AddToLog(-1, "Read line " & i + 1 & "; Name: " & name(i) & " weight: " & w)
                weight(i - i0) = ValueConverter.StrToDbl(w)
            Next i
            If IsNumeric(str(0)) Then
                LordMinWeight = ValueConverter.StrToDbl(str(0))
            Else
                LordMinWeight = 0.49 * weight.Max
            End If
            AddToLog(-1, "LordMinWeight = " & LordMinWeight)
            t = IO.File.GetCreationTime(path)
        Catch ex As Exception
            AddToLog(-1, ex.Message)
        End Try
        AddToLog(-1, "End reading")
        Return t
    End Function
    Private Sub ReadExclusions()
        excludeRace.Add(comm.RaceIdentifierToSubrace("Greenskins"))
        Dim str() As String = comm.TxtSplit(My.Resources.ExcludeIDsForNames)
        For i As Integer = 0 To UBound(str) Step 1
            exclude.Add(str(i).ToUpper)
        Next i
    End Sub

    '''<summary>Присвоит имя отряду</summary>
    ''' <param name="stack">Уже сгенерированные стэки</param>
    ''' <param name="R">Инициализированный класс</param>
    ''' <param name="newMapGen">True, если это первый вызов этой функции при генерации новой карты</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Sub GenName(ByRef stack As AllDataStructues.Stack, ByRef R As RandStack, ByVal newMapGen As Boolean, _
                       Optional ByVal LogID As Integer = -1)
        If IsNothing(stack) Or IsNothing(R) Then Exit Sub
        If Not IsNothing(name) And newMapGen Then
            AddToLog(LogID, "Refreshing stack names list")
            commonIDs.Clear()
            For i As Integer = 0 To UBound(name) Step 1
                commonIDs.Add(i)
            Next i
        End If
        If stack.leaderPos > -1 AndAlso Not IsNothing(stack.pos) Then
            stack.name = SetName(stack.pos(stack.leaderPos).ToUpper, R, LogID)
        Else
            stack.name = ""
        End If
    End Sub
    Private Function SetName(ByRef leaderID As String, ByRef R As RandStack, ByRef LogID As Integer) As String
        Dim u As AllDataStructues.Unit = R.FindUnitStats(leaderID)
        Dim res As String = u.name
        If excludeRace.Contains(u.race) Then Return res '& " race_excluded"
        If exclude.Contains(u.unitID) Then Return res '& " unit_excluded"
        If commonIDs.Count > 0 AndAlso rndgen.PRand(0, 1) > 0.9 Then
            Dim i As Integer = comm.RandomSelection(commonIDs, weight, True)
            commonIDs.Remove(i)
            res &= " " & name(i)
            res = NameCut(res, StackNameMaxLen)
            AddToLog(LogID, "Using name: " & name(i))
        End If
        Return res
    End Function
    Private Function NameCut(ByRef input As String, ByRef maxLen As Integer) As String
        If input.Length > maxLen Then
            Return input.Substring(0, maxLen)
        Else
            Return input
        End If
    End Function

    '''<summary>Присвоит имя лорду</summary>
    ''' <param name="RaceID">ID расы лорда</param>
    ''' <param name="newMapGen">True, если это первый вызов этой функции при генерации новой карты</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Function LordName(ByRef RaceID As Integer, ByVal newMapGen As Boolean, _
                             Optional ByVal LogID As Integer = -1) As String
        If Not IsNothing(name) And newMapGen Then
            lordIDs.Clear()
            For i As Integer = 0 To UBound(name) Step 1
                If weight(i) > LordMinWeight Then lordIDs.Add(i)
            Next i
        End If
        If newMapGen Then
            AddToLog(LogID, "Refreshing lord names list")
            defailtLords.Clear()
            For i As Integer = 0 To UBound(defaultLordNames) Step 1
                If Not IsNothing(defaultLordNames(i)) Then
                    defailtLords.Add(i, New List(Of Integer))
                    For j As Integer = 0 To UBound(defaultLordNames(i)) Step 1
                        defailtLords.Item(i).Add(j)
                    Next j
                End If
            Next i
        End If
        Dim result As String
        If lordIDs.Count > 0 AndAlso rndgen.PRand(0, 1) > 0.5 Then
            Dim i As Integer = comm.RandomSelection(lordIDs, weight, True)
            lordIDs.Remove(i)
            result = name(i)
        Else
            If defailtLords.Item(RaceID).Count = 0 Then Throw New Exception("Lords names list is empty. Race ID: " & RaceID)
            Dim i As Integer = comm.RandomSelection(defailtLords.Item(RaceID), True)
            defailtLords.Item(RaceID).Remove(i)
            result = defaultLordNames(RaceID)(i)
        End If
        Dim LName As String = NameCut(result, LordNameMaxLen)
        AddToLog(LogID, "Selected lord name for race " & comm.RaceNumberToRaceChar(RaceID) & ": " & LName)
        Return LName
    End Function

    Private Sub AddToLog(ByRef LogID As Integer, ByRef Msg As String)
        If LogID > -1 Then
            Call log.MAdd(LogID, Msg)
        Else
            Call log.Add(Msg)
        End If
    End Sub

End Class
