Public Class PenetrableObjectsGen

    Public Sub Gen(ByRef m As Map, ByRef settMap As Map.SettingsMap)

        If Not m.complited.ImpenetrableObjectsPlacing_Done Then
            Throw New Exception("Сначала нужно выполнить RaceGen.Gen")
        End If

        Dim t0 As Integer = Environment.TickCount

        Call GenRoads(m, settMap)
        Call GenForest(m, settMap)

        Dim t1 As Integer = Environment.TickCount
        Console.WriteLine("Penetrable objects placing: " & t1 - t0)

        m.complited.PenetrableObjectsPlacing_Done = True

    End Sub

    Private Sub GenRoads(ByRef m As Map, ByRef settMap As Map.SettingsMap)

    End Sub

    Private Sub GenForest(ByRef m As Map, ByRef settMap As Map.SettingsMap)

    End Sub

End Class
