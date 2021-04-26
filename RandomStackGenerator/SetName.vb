Public Class SetName

    Private link As String = "https://norvezskayasemga.wixsite.com/d2bfwmod/donaters"
    Private path As String = ".\Donaters.txt"
    Private users() As Donater = Nothing
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
    Public preferedUnit As New Dictionary(Of String, String)
    Private Const chanceToAddNameToStack As Double = 0.125 ' (0,1]

    ''' <summary>Сюда генератор пишет лог</summary>
    Public log As Log

    Private Structure Donater
        Dim name As String
        Dim unit As String
        Dim weight As Double
        Dim isUser As Boolean

        Dim increasedWeight As Double
        Dim decreasedWeight As Double
        Dim unitsCount As Integer
    End Structure

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
        If IsNothing(users) Then
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

        ReDim Preserve users(users.Length)
        users(UBound(users)) = New Donater With {.name = "!!!NameBug!!!", .unit = "", .weight = 0, .isUser = False}
        For i As Integer = 0 To UBound(users) - 1 Step 1
            users(i).isUser = True
            users(UBound(users)).weight += users(i).weight
        Next i
        users(UBound(users)).weight = users(UBound(users)).weight / chanceToAddNameToStack - users(UBound(users)).weight

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
                ReDim users(UBound(Tn))
                For i As Integer = 0 To UBound(Tn) Step 1
                    If Not IsNothing(Tn(i)) AndAlso Not Tn(i) = "" AndAlso Tw(i) > 0 Then
                        Dim s As String = Tn(i).ToUpper
                        If Not added.ContainsKey(s) Then
                            users(added.Count).name = Tn(i)

                            MsgBox("нужно обновить таблицу на сайте и переделать под нее парсер")
                            users(added.Count).unit = GenDefaultValues.emptyItem
                            added.Add(s, added.Count)
                        End If
                        users(added.Item(s)).weight += Tw(i)
                    End If
                Next i
                ReDim Preserve users(added.Count - 1)
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
            If IsNothing(users) Then Exit Sub
            Dim str As String = LordMinWeight.ToString
            For i As Integer = 0 To UBound(users) Step 1
                If users(i).isUser Then
                    str &= vbNewLine & users(i).name & vbTab & users(i).weight & vbTab & users(i).unit
                End If
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
            ReDim users(UBound(str) - i0)
            Dim wmax As Double
            For i As Integer = i0 To UBound(users) + i0 Step 1
                s = str(i).Split(CChar(" "))
                For j As Integer = 0 To UBound(s) - 2 Step 1
                    users(i - i0).name &= s(j)
                    If j < UBound(s) - 1 Then users(i - i0).name &= " "
                Next j
                w = s(UBound(s) - 1)
                users(i - i0).unit = s(UBound(s))
                AddToLog(-1, "Read line " & i + 1 & "; Name: " & users(i - i0).name & " weight: " & w & " unit: " & users(i - i0).unit)
                users(i - i0).weight = ValueConverter.StrToDbl(w)
                wmax = Math.Max(wmax, users(i - i0).weight)
            Next i
            If IsNumeric(str(0)) Then
                LordMinWeight = ValueConverter.StrToDbl(str(0))
            Else
                LordMinWeight = 0.49 * wmax
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

    '''<summary>Присвоит имя отрядам</summary>
    ''' <param name="stack">Уже сгенерированные стэки</param>
    ''' <param name="R">Инициализированный класс</param>
    ''' <param name="newMapGen">True, если это первый вызов этой функции при генерации новой карты</param>
    ''' <param name="LogID">Номер задачи. От 0 до Size-1. Если меньше 0, запись будет сделана в общий лог</param>
    Public Sub GenName(ByRef stack() As AllDataStructues.Stack, ByRef R As RandStack, ByVal newMapGen As Boolean, _
                       Optional ByVal LogID As Integer = -1)
        If IsNothing(stack) Or IsNothing(R) Then Exit Sub
        Call ResetNames(newMapGen, LogID)

        Dim possibleNames As New List(Of String)
        Dim leaderID(UBound(stack)) As String
        Dim skip(UBound(stack)) As Boolean
        Dim ids(UBound(stack)), i As Integer
        For k As Integer = 0 To UBound(stack) Step 1
            ids(k) = k
        Next k
        Call rndgen.Shuffle(ids)

        Dim uCount As New Dictionary(Of String, Integer())
        For k As Integer = 0 To UBound(stack) Step 1
            i = ids(k)
            If Not IsNothing(stack(i)) Then
                If stack(i).leaderPos > -1 AndAlso Not IsNothing(stack(i).pos) Then
                    Dim unitID As String = stack(i).pos(stack(i).leaderPos).ToUpper
                    If Not uCount.ContainsKey(unitID) Then uCount.Add(unitID, New Integer() {0})
                    uCount.Item(unitID)(0) += 1
                Else
                    stack(i).name = ""
                    skip(i) = True
                End If
            Else
                skip(i) = True
            End If
        Next k
        Call calcUWeigt(uCount)

        For k As Integer = 0 To UBound(stack) Step 1
            i = ids(k)
            If Not skip(i) Then
                stack(i).name = SetName(stack(i).pos(stack(i).leaderPos).ToUpper, R, LogID)
                skip(i) = True
            End If
        Next k
    End Sub
    Private Function SetName(ByRef leaderID As String, ByRef R As RandStack, ByRef LogID As Integer) As String
        Dim u As AllDataStructues.Unit = R.FindUnitStats(leaderID)
        Dim res As String = u.name
        Dim i As Integer = comm.RandomSelection(commonIDs, WeightArray(u.unitID), True)
        If Not users(i).isUser Then Return res

        commonIDs.Remove(i)
        If Not u.unitID.ToUpper = users(i).unit.ToUpper Then
            If excludeRace.Contains(u.race) Then Return res '& " race_excluded"
            If exclude.Contains(u.unitID) Then Return res '& " unit_excluded"
        End If
        res &= " " & users(i).name
        res = NameCut(res, StackNameMaxLen)
        AddToLog(LogID, "Using name: " & users(i).name)
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
            Dim i As Integer = comm.RandomSelection(lordIDs, WeightArray, True)
            lordIDs.Remove(i)
            result = users(i).name
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
    Private Function WeightArray(Optional ByRef unitID As String = "") As Double()
        Dim weight(UBound(users)) As Double
        For w As Integer = 0 To UBound(users) Step 1
            If unitID = "" Then
                weight(w) = users(w).weight
            ElseIf users(w).unit.ToUpper = unitID.ToUpper Then
                weight(w) = users(w).increasedWeight
            Else
                weight(w) = users(w).decreasedWeight
            End If
        Next w
        Return weight
    End Function
    Private Sub calcUWeigt(ByRef uCount As Dictionary(Of String, Integer()))
        Dim stacksCount As Integer = 0
        If Not IsNothing(uCount)
            For Each i As Integer() In uCount.Values
                stacksCount += i(0)
            Next i
        End If
        Dim minWeight As Double
        Dim desiredUnitCount As Integer
        For i As Integer = 0 To UBound(users) Step 1
            If users(i).isUser AndAlso Not IsNothing(uCount) AndAlso uCount.ContainsKey(users(i).unit.ToUpper) Then
                desiredUnitCount = uCount.Item(users(i).unit.ToUpper)(0)
                minWeight = 0.5 * users(i).weight
                users(i).unitsCount = desiredUnitCount

                users(i).increasedWeight = 3 * users(i).weight
                users(i).decreasedWeight = (users(i).weight * stacksCount - users(i).increasedWeight * desiredUnitCount) / Math.Max(1, stacksCount - desiredUnitCount)
                If users(i).decreasedWeight < minWeight Then
                    users(i).decreasedWeight = minWeight
                    users(i).increasedWeight = (users(i).weight * stacksCount - (stacksCount - desiredUnitCount) * users(i).decreasedWeight) / desiredUnitCount
                End If
            Else
                users(i).increasedWeight = users(i).weight
                users(i).decreasedWeight = users(i).weight
            End If
        Next i
    End Sub

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
            Call calcUWeigt(Nothing)
            Call ResetUsedStackNames(LogID)
            Call ResetLordsNames(LogID)
        End If
    End Sub
    Private Sub ResetUsedStackNames(ByRef LogID As Integer)
        If Not IsNothing(users) Then
            AddToLog(LogID, "Refreshing stack names list")
            commonIDs.Clear()
            For i As Integer = 0 To UBound(users) Step 1
                commonIDs.Add(i)
            Next i
        End If
    End Sub
    Private Sub ResetLordsNames(ByRef LogID As Integer)
        AddToLog(LogID, "Refreshing lords names list")

        If Not IsNothing(users) Then
            lordIDs.Clear()
            For i As Integer = 0 To UBound(users) Step 1
                If users(i).isUser And users(i).weight > LordMinWeight Then lordIDs.Add(i)
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
