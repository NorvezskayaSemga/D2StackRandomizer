Public Class SetName

    Private link As String = "https://norvezskayasemga.wixsite.com/d2bfwmod/donaters"
    Private path As String = ".\Donaters.txt"
    Private name() As String = Nothing
    Private weight() As Double = Nothing
    Private comm As New Common
    Private rndgen As New RndValueGen
    Private exclude As New List(Of String)
    Private excludeRace As New List(Of Integer)
    Friend Const StackNameMaxLen As Integer = 30
    Friend Const LordNameMaxLen As Integer = 15
    Friend Const CityNameMaxLen As Integer = 20
    Friend Const CapitalNameMaxLen As Integer = 20
    Friend Const MerchantNameMaxLen As Integer = 35
    Friend Const RuinsNameMaxLen As Integer = 25
    Friend Const DescriptionMaxLen As Integer = 100
    Private commonIDs, lordIDs As New List(Of Integer)
    Private defailtLords As New Dictionary(Of Integer, List(Of Integer))
    Private LordMinWeight As Double
    Private defaultLordNames(14)() As String
    Private lordDonationThreshold As Double = 999
    Public customObjectsNames As GenDefaultValues.MapObjectsText

    ''' <summary>Сюда генератор пишет лог</summary>
    Public log As Log

    Public Sub New(ByVal lang As GenDefaultValues.TextLanguage)

        log = New Log(comm)

        customObjectsNames = New GenDefaultValues.MapObjectsText(lang, comm.defValues)

        Call log.Enable()
        Call AddToLog(-1, "-----Names creator initialization started-----")

        For Each k As String In comm.LordsRace.Keys
            If IsNothing(defaultLordNames(comm.LordsRace.Item(k))) Then
                defaultLordNames(comm.LordsRace.Item(k)) = customObjectsNames.LordNamesArray(k)
            End If
        Next k

        Dim t As Date = ReadNames()
        Dim download As Boolean = False
        If IsNothing(name) Then
            download = True
        Else
            Dim d As Integer = 365 * (DateAndTime.Today.Year - t.Year) + DateAndTime.Today.DayOfYear - t.DayOfYear
            If d > 7 Then download = True
        End If
        If download Then
            Call DownloadList()
            Call PrintNames()
        End If
        Call ReadExclusions()

        Call AddToLog(-1, "-----Names creator initialization ended-----")
    End Sub

    Private Sub DownloadList()

        Dim w As System.Net.WebClient = Nothing
        Dim Tn() As String = Nothing
        Dim Tw() As Double = Nothing
        Try
            Net.ServicePointManager.SecurityProtocol = CType(3072, Net.SecurityProtocolType) Or _
                                                       CType(768, Net.SecurityProtocolType) Or _
                                                       CType(192, Net.SecurityProtocolType) Or _
                                                       CType(48, Net.SecurityProtocolType)

            w = New System.Net.WebClient With {.Proxy = Nothing}
            Dim byteData() As Byte = w.DownloadData(link)
            Dim d As String = Text.Encoding.UTF8.GetString(byteData)
            Dim str() As String = d.Replace(Chr(10), vbNewLine).Split(CChar(vbNewLine))
            Dim base As String = ""
            For s As Integer = 0 To UBound(str) Step 1
                If str(s).Contains("!-- warmup data start") Then
                    base = str(s + 1)
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
                'вот здесь нужно корректно обработать случай, если в базе данных есть пустые записи
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
            Console.WriteLine(ex.Message)
            AddToLog(-1, "Names downloader: " & ex.Message)
        End Try
        Try
            If Not IsNothing(Tn) And Not IsNothing(Tw) Then
                Dim added As New Dictionary(Of String, Integer)
                ReDim name(UBound(Tn)), weight(UBound(Tw))
                For i As Integer = 0 To UBound(Tn) Step 1
                    If Not IsNothing(Tn(i)) AndAlso Not Tn(i) = "" AndAlso Tw(i) > 0 Then
                        Dim s As String = Tn(i).ToUpper
                        If Not added.ContainsKey(s) Then
                            name(added.Count) = Tn(i)
                            added.Add(s, added.Count)
                        End If
                        weight(added.Item(s)) += Tw(i)
                    End If
                Next i
                ReDim Preserve name(added.Count - 1), weight(added.Count - 1)
            End If
        Catch ex As Exception
            Console.WriteLine(ex.Message)
            AddToLog(-1, "Names weight sum: " & ex.Message)
        End Try
        Try
            If Not IsNothing(w) Then w.Dispose()
        Catch ex As Exception
            AddToLog(-1, "Web client dispose: " & ex.Message)
        End Try
    End Sub

    Private Sub PrintNames()
        Try
            If IsNothing(name) Then Exit Sub
            Dim str As String = LordMinWeight.ToString
            For i As Integer = 0 To UBound(name) Step 1
                str &= vbNewLine & name(i) & vbTab & weight(i)
            Next i
            AddToLog(-1, "Writing " & path)
            IO.File.WriteAllText(path, str)
            AddToLog(-1, "End writing")
        Catch ex As Exception
            AddToLog(-1, ex.Message)
        End Try
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
            For i As Integer = i0 To UBound(name) + i0 Step 1
                s = str(i).Split(CChar(" "))
                For j As Integer = 0 To UBound(s) - 1 Step 1
                    name(i - i0) &= s(j)
                    If j < UBound(s) - 1 Then name(i - i0) &= " "
                Next j
                w = s(UBound(s))
                AddToLog(-1, "Read line " & i + 1 & "; Name: " & name(i - i0) & " weight: " & w)
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
        Try
            excludeRace.Add(comm.RaceIdentifierToSubrace("Greenskins"))
            Dim str() As String = comm.TxtSplit(comm.defValues.ExcludeIDsForNames)
            log.Add(comm.ReadingLog.PrintAll)
            For i As Integer = 0 To UBound(str) Step 1
                exclude.Add(str(i).ToUpper)
            Next i
        Catch ex As Exception
            Console.WriteLine(ex.Message)
            AddToLog(-1, "Exclusions reader: " & ex.Message)
        End Try
    End Sub

    '''<summary>Присвоит имя отряду</summary>
    ''' <param name="stack">Уже сгенерированные стэки</param>
    ''' <param name="R">Инициализированный класс</param>
    ''' <param name="newMapGen">True, если это первый вызов этой функции при генерации новой карты</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Sub GenName(ByRef stack As AllDataStructues.Stack, ByRef R As RandStack, ByVal newMapGen As Boolean, _
                       Optional ByVal LogID As Integer = -1)
        If IsNothing(stack) Or IsNothing(R) Then Exit Sub
        Call ResetNames(newMapGen, LogID)

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
        Call ResetNames(newMapGen, LogID)

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
        AddToLog(LogID, "Selected lord name for race " & comm.defValues.RaceNumberToRaceChar(RaceID) & ": " & LName)
        Return LName
    End Function

    Public Function CityName(ByRef ID As String, ByRef RaceID As Integer) As String
        If comm.defValues.capitalToGeneratorRace.ContainsKey(ID.ToUpper) Then
            Return customObjectsNames.GetCapitalName(comm.defValues.generatorRaceToCapitalID(comm.defValues.RaceNumberToRaceChar(RaceID)))
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyTown) Then
            Return customObjectsNames.GetCityName(ID.Remove(ID.Length - 1) & "@" & comm.defValues.RaceNumberToRaceChar(RaceID))
        Else
            Throw New Exception("Unknown object ID: " & ID)
        End If
    End Function
    Public Function ObjectName(ByRef ID As String) As String
        If ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyRuin) Then
            Return customObjectsNames.GetRuinName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyMerchant) Then
            Return customObjectsNames.GetVendorName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyMage) Then
            Return customObjectsNames.GetMageName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyMercenaries) Then
            Return customObjectsNames.GetMercenaryName(ID)
        ElseIf ID.ToUpper.StartsWith(GenDefaultValues.wObjKeyTrainer) Then
            Return customObjectsNames.GetTrainerName(ID)
        Else
            Throw New Exception("Unknown object ID: " & ID)
        End If
    End Function
    Public Function ObjectDescription(ByRef ID As String) As String
        Return customObjectsNames.GetDescription(ID)
    End Function

    Friend Sub ResetNames(ByRef newMapGen As Boolean, ByRef LogID As Integer)
        If newMapGen Then
            Call ResetUsedStackNames(LogID)
            Call ResetLordsNames(LogID)
        End If
    End Sub
    Private Sub ResetUsedStackNames(ByRef LogID As Integer)
        If Not IsNothing(name) Then
            AddToLog(LogID, "Refreshing stack names list")
            commonIDs.Clear()
            For i As Integer = 0 To UBound(name) Step 1
                commonIDs.Add(i)
            Next i
        End If
    End Sub
    Private Sub ResetLordsNames(ByRef LogID As Integer)
        AddToLog(LogID, "Refreshing lords names list")

        If Not IsNothing(name) Then
            lordIDs.Clear()
            For i As Integer = 0 To UBound(name) Step 1
                If weight(i) > LordMinWeight Then lordIDs.Add(i)
            Next i
        End If

        defailtLords.Clear()
        For i As Integer = 0 To UBound(defaultLordNames) Step 1
            If Not IsNothing(defaultLordNames(i)) Then
                defailtLords.Add(i, New List(Of Integer))
                For j As Integer = 0 To UBound(defaultLordNames(i)) Step 1
                    defailtLords.Item(i).Add(j)
                Next j
            End If
        Next i
    End Sub

    Private Sub AddToLog(ByRef LogID As Integer, ByRef Msg As String)
        If LogID > -1 Then
            Call log.MAdd(LogID, Msg)
        Else
            Call log.Add(Msg)
        End If
    End Sub

End Class
