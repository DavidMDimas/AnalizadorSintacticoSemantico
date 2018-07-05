Imports System.Text.RegularExpressions
Imports System.IO
Imports System.Text



Public Class Form1

    Private _label As Object

    Private Property Label(p1 As Integer) As Object
        Get
            Return _label
        End Get
        Set(value As Object)
            _label = value
        End Set
    End Property

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        dlgOpenFile.InitDir = Application.StartupPath


    End Sub

    Dim archivo, tamaño_1
    Const alfabeto = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789#<>.+-/*%=<><=>=!!=<>^||&&#,.()"
    Const simbolos = "# , . ( ) : ; [ ] { } & Chr(91)"
    Dim contespacio As Integer, conenter As Integer
    Dim letraenc, espacio, FINAL, coma, FIN As Boolean

    Const pala_reser = "JETS INT FLOAT CHAR GETCH CONIO DOS STRINGS STDLIB MATCH SHORT LONG DOUBLE CLASS PUBLIC STATIC VOID MAIN FOR IF WHILE DO SWITCH CASE PRINTF SCANF GOTOXY RETURN INLCUDE ELSE GETS CLRSCR clrscr puts GETCH FINAL"
    Const librerias = "#INCLUDE<DOS.H> #INCLUDE<STDIO.H> #INCLUDE<CONIO.H> #INCLUDE<STRING.H> #INCLUDE<STDLIB.H>"
    Const pala = "a b c d e f g h i j k l m n o p q r s t u v w x y z"
    Const numeros = "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40"
    Const ope_arit = "+ - * / % "
    Const ope_rela = "= < ! > <= >= != <>"
    Const ope_logi = " ^ || &&"
    Const identify = "Espacio"
    Const comillas = " "" "
    Const identificadores = "%D %F %I %S"
    Dim tama As Integer, val As Integer, val1 As Integer, val2 As Integer, val3 As Integer, val4 As Integer, val5 As Integer, val6 As Integer, val7 As Integer, val8 As Integer, val9 As Integer, val10 As Integer, tamca As Integer
    Dim analizador As String, palfabeto As String, rego As String, regd As String
    Dim FileName As String
    Dim cadenaTxt As String
    Dim letra As String
    Dim TodoTexto As String = "", LineaDeTexto As String = ""
    Dim cadena, cadena2, alm, mensaje As String

    Dim linea



    Private Sub Cargar_Click(sender As Object, e As EventArgs) Handles Cargar.Click
        Dim clineas As Integer = ListBox1.Items.Count

        Text21.Visible = True
        ListBox3.Visible = True
        CargarTexto()
        descomponer()
        n_linea.Text = clineas

     
    End Sub

    Private Sub descomponer()
        Dim s, s1, bus As String
        Dim i As Integer


        s = Text21.Text

        For i = 1 To Len(s)
            bus = Mid(Mid(s, 1, i), i, 1)

            If (bus = " ") Or (bus = "") Or (bus = Chr(10)) Then
                ListBox3.Items.Add(s1.Trim())
                s1 = "".Trim()
                ListBox3.Items.Add("Espacio".Trim())
            Else

                s1 = s1 + bus

            End If

        Next
        ListBox3.Items.Add("(Final)".Trim())

    End Sub

    Private Sub CargarTexto()

        Dim OpenFileDialog1 As New OpenFileDialog
        Dim x As Integer = 1
       

        OpenFileDialog1.Filter = "Formato de archivo (*.TXT)|*.TXT"
        OpenFileDialog1.ShowDialog()
        If OpenFileDialog1.FileName <> "" Then
            Try
                FileOpen(1, OpenFileDialog1.FileName, OpenMode.Input)
                Do Until EOF(1)
                    LineaDeTexto = LineInput(1)
                    For x = 1 To 1
                        TodoTexto = TodoTexto & LineaDeTexto & vbCrLf
                        ListBox1.Items.Add(LineaDeTexto)

                    Next x
                    'creartabla(x)
                Loop
                Text21.Text = TodoTexto
                Text21.Enabled = True
                Text21.ForeColor = Color.Red
            Catch
                MsgBox("No ha seleccionado ningún archivo")
            Finally
                FileClose(1)
            End Try
        End If


    End Sub

   


    Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox1.SelectedIndexChanged

    End Sub
    Private Sub Button21_Click(sender As Object, e As EventArgs) Handles Button21.Click
        Dim x, y, w, z, a, b, c, d, f, g, h, k, l, m, n, o, p, q, r, s As String
        Dim lineas As Integer = ListBox1.Items.Count
        Dim l_case As Integer

        c_eleme.Text = lineas
        l_case = c_eleme.Text
        'l_case = 20
        ' n_linea.Visible = True
        ListBox2.Visible = True



        Select Case l_case

            Case 1
                x = ListBox1.Items(0)
                Text1.Text = x
            Case 2
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                Text1.Text = x
                TextBox1.Text = w
            Case 3
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
            Case 4
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
            Case 5
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
            Case 6
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
            Case 7
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
            Case 8
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
            Case 9
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
            Case 10
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
            Case 11
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
            Case 12
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
            Case 13
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
            Case 14
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                m = ListBox1.Items(13)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
                TextBox13.Text = m
            Case 15
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                m = ListBox1.Items(13)
                n = ListBox1.Items(14)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
                TextBox13.Text = m
                TextBox14.Text = n
            Case 16
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                m = ListBox1.Items(13)
                n = ListBox1.Items(14)
                o = ListBox1.Items(15)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
                TextBox13.Text = m
                TextBox14.Text = n
                TextBox15.Text = o
            Case 17
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                m = ListBox1.Items(13)
                n = ListBox1.Items(14)
                o = ListBox1.Items(15)
                p = ListBox1.Items(16)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
                TextBox13.Text = m
                TextBox14.Text = n
                TextBox15.Text = o
                TextBox16.Text = p
            Case 18
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                m = ListBox1.Items(13)
                n = ListBox1.Items(14)
                o = ListBox1.Items(15)
                p = ListBox1.Items(16)
                q = ListBox1.Items(17)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
                TextBox13.Text = m
                TextBox14.Text = n
                TextBox15.Text = o
                TextBox16.Text = p
                TextBox17.Text = q
            Case 19
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                m = ListBox1.Items(13)
                n = ListBox1.Items(14)
                o = ListBox1.Items(15)
                p = ListBox1.Items(16)
                q = ListBox1.Items(17)
                r = ListBox1.Items(18)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
                TextBox13.Text = m
                TextBox14.Text = n
                TextBox15.Text = o
                TextBox16.Text = p
                TextBox17.Text = q
                TextBox18.Text = r
            Case 20
                x = ListBox1.Items(0)
                w = ListBox1.Items(1)
                z = ListBox1.Items(2)
                y = ListBox1.Items(3)
                a = ListBox1.Items(4)
                b = ListBox1.Items(5)
                c = ListBox1.Items(6)
                d = ListBox1.Items(7)
                f = ListBox1.Items(8)
                g = ListBox1.Items(9)
                h = ListBox1.Items(10)
                k = ListBox1.Items(11)
                l = ListBox1.Items(12)
                m = ListBox1.Items(13)
                n = ListBox1.Items(14)
                o = ListBox1.Items(15)
                p = ListBox1.Items(16)
                q = ListBox1.Items(17)
                r = ListBox1.Items(18)
                s = ListBox1.Items(19)
                Text1.Text = x
                TextBox1.Text = w
                TextBox2.Text = z
                TextBox3.Text = y
                TextBox4.Text = a
                TextBox5.Text = b
                TextBox6.Text = c
                TextBox7.Text = d
                TextBox8.Text = f
                TextBox9.Text = g
                TextBox10.Text = h
                TextBox11.Text = k
                TextBox12.Text = l
                TextBox13.Text = m
                TextBox14.Text = n
                TextBox15.Text = o
                TextBox16.Text = p
                TextBox17.Text = q
                TextBox18.Text = r
                TextBox19.Text = s
            Case Else
                MsgBox("Solo hay espacio para 20 lineas de codigo", vbCritical, "Error Fatal")
        End Select
        mMostrar()

    End Sub

    Private Sub Button22_Click(sender As Object, e As EventArgs) Handles Button22.Click
        Dim a_sin As Integer

        mMostrar()
        ListBox2.Visible = True
        a_sin = c_eleme.Text
        For i = 0 To a_sin - 1
            FuTodo(i)
        Next i


    End Sub
    Private Sub Lexico_Click(sender As Object, e As EventArgs) Handles Lexico.Click

        Dim cantidaditems As Integer = ListBox3.Items.Count - 1
        txtCantidadE.Text = cantidaditems


        For i = 0 To cantidaditems
            analizador = ListBox3.Items(i)
            val = InStr(1, pala_reser, analizador)
            val1 = InStr(1, simbolos, analizador)
            val2 = InStr(1, identify, analizador)
            val3 = InStr(1, librerias, analizador)
            val4 = InStr(1, ope_arit, analizador)
            val5 = InStr(1, ope_rela, analizador)
            val6 = InStr(1, ope_logi, analizador)
            val7 = InStr(1, pala, analizador)
            val8 = InStr(1, numeros, analizador)
            val9 = InStr(1, comillas, analizador)
            val10 = InStr(1, identificadores, analizador)

            If val <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Palabra reservada"
            ElseIf val3 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Es una Libreria"
            ElseIf val1 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Es un simbolo"
            ElseIf val2 <> 0 Then
                ListBox3.Items(i) = ""
            ElseIf val4 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Es un Operador Aritmetico"
            ElseIf val5 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Es un operador Relacional"
            ElseIf val6 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Es un operador logico"
            ElseIf val7 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Letras pero tambien puede ser variable"
            ElseIf val8 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Es un numero"
            ElseIf val9 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Comillas"
            ElseIf val10 <> 0 Then
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Es un Identificador"
            Else
                ListBox3.Items(i) = ListBox3.Items(i) & "              /Cadena pero tambien puede ser una variable "

            End If
        Next
    End Sub

 
    Private Sub btnTabla_Click(sender As Object, e As EventArgs)

    End Sub



    Private Sub Text21_TextChanged(sender As Object, e As EventArgs) Handles Text21.TextChanged

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        linea = 0
        FuTodo(linea)
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        linea = 1
        FuTodo(linea)
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        linea = 2
        FuTodo(linea)
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        linea = 3
        FuTodo(linea)
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        linea = 4
        FuTodo(linea)
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        linea = 5
        FuTodo(linea)
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        linea = 6
        FuTodo(linea)
    End Sub

    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        linea = 7
        FuTodo(linea)
    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        linea = 8
        FuTodo(linea)
       
    End Sub

    Private Sub Button10_Click(sender As Object, e As EventArgs) Handles Button10.Click
        linea = 9
        FuTodo(linea)
    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles Button11.Click
        linea = 10
        FuTodo(linea)
    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles Button12.Click
        linea = 11
        FuTodo(linea)
    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs) Handles Button13.Click
        linea = 12
        FuTodo(linea)
    End Sub

    Private Sub Button14_Click(sender As Object, e As EventArgs) Handles Button14.Click
        linea = 13
        FuTodo(linea)
    End Sub

    Private Sub Button15_Click(sender As Object, e As EventArgs) Handles Button15.Click
        linea = 14
        FuTodo(linea)
    End Sub

    Private Sub Button16_Click(sender As Object, e As EventArgs) Handles Button16.Click
        linea = 15
        FuTodo(linea)
    End Sub

    Private Sub Button17_Click(sender As Object, e As EventArgs) Handles Button17.Click
        linea = 16
        FuTodo(linea)
    End Sub

    Private Sub Button18_Click(sender As Object, e As EventArgs) Handles Button18.Click
        linea = 17
        FuTodo(linea)
    End Sub

    Private Sub Button19_Click(sender As Object, e As EventArgs) Handles Button19.Click
        linea = 18
        FuTodo(linea)
    End Sub
    Private Sub Button20_Click(sender As Object, e As EventArgs) Handles Button20.Click
        linea = 19
        FuTodo(linea)
    End Sub



    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles Text1.TextChanged

    End Sub

    
    Private Sub MyTextBox_TextChanged(sender As Object, e As EventArgs)

    End Sub


    Private Sub btnContar_Click(sender As Object, e As EventArgs) Handles btnContar.Click

    End Sub

    Private Sub ListBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox3.SelectedIndexChanged

    End Sub

    Private Sub mMostrar()
        Dim lineas As Integer = ListBox1.Items.Count
        Dim l_case As Integer

        c_eleme.Text = lineas
        l_case = c_eleme.Text
        'l_case = 20
        ' n_linea.Visible = True




        Select Case l_case
            Case 1
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                ListBox2.Height = 25
            Case 2
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                ListBox2.Height = 50
            Case 3
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                ListBox2.Height = 75
            Case 4
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                ListBox2.Height = 100
            Case 5
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                ListBox2.Height = 125
            Case 6
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                ListBox2.Height = 150
            Case 7
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                ListBox2.Height = 175
            Case 8
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                ListBox2.Height = 200
            Case 9
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                ListBox2.Height = 225
            Case 10
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                ListBox2.Height = 250
            Case 11
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                ListBox2.Height = 275
            Case 12
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                ListBox2.Height = 300
            Case 13
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                ListBox2.Height = 325
            Case 14
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                TextBox13.Visible = True
                Label_14.Visible = True
                Label14.Visible = True
                Button14.Visible = True
                ListBox2.Height = 350
            Case 15
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                TextBox13.Visible = True
                Label_14.Visible = True
                Label14.Visible = True
                Button14.Visible = True
                TextBox14.Visible = True
                Label_15.Visible = True
                Label15.Visible = True
                Button15.Visible = True
                ListBox2.Height = 375
            Case 16
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                TextBox13.Visible = True
                Label_14.Visible = True
                Label14.Visible = True
                Button14.Visible = True
                TextBox14.Visible = True
                Label_15.Visible = True
                Label15.Visible = True
                Button15.Visible = True
                TextBox15.Visible = True
                Label_16.Visible = True
                Label16.Visible = True
                Button16.Visible = True
                ListBox2.Height = 400
            Case 17
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                TextBox13.Visible = True
                Label_14.Visible = True
                Label14.Visible = True
                Button14.Visible = True
                TextBox14.Visible = True
                Label_15.Visible = True
                Label15.Visible = True
                Button15.Visible = True
                TextBox15.Visible = True
                Label_16.Visible = True
                Label16.Visible = True
                Button16.Visible = True
                TextBox16.Visible = True
                Label_17.Visible = True
                Label17.Visible = True
                Button17.Visible = True
                ListBox2.Height = 425
            Case 18
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                TextBox13.Visible = True
                Label_14.Visible = True
                Label14.Visible = True
                Button14.Visible = True
                TextBox14.Visible = True
                Label_15.Visible = True
                Label15.Visible = True
                Button15.Visible = True
                TextBox15.Visible = True
                Label_16.Visible = True
                Label16.Visible = True
                Button16.Visible = True
                TextBox16.Visible = True
                Label_17.Visible = True
                Label17.Visible = True
                Button17.Visible = True
                TextBox17.Visible = True
                Label_18.Visible = True
                Label18.Visible = True
                Button18.Visible = True
                ListBox2.Height = 450
            Case 19
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                TextBox13.Visible = True
                Label_14.Visible = True
                Label14.Visible = True
                Button14.Visible = True
                TextBox14.Visible = True
                Label_15.Visible = True
                Label15.Visible = True
                Button15.Visible = True
                TextBox15.Visible = True
                Label_16.Visible = True
                Label16.Visible = True
                Button16.Visible = True
                TextBox16.Visible = True
                Label_17.Visible = True
                Label17.Visible = True
                Button17.Visible = True
                TextBox17.Visible = True
                Label_18.Visible = True
                Label18.Visible = True
                Button18.Visible = True
                TextBox18.Visible = True
                Label_19.Visible = True
                Label19.Visible = True
                Button19.Visible = True
                ListBox2.Height = 475
            Case 20
                Text1.Visible = True
                Label_1.Visible = True
                Label1.Visible = True
                Button1.Visible = True
                TextBox1.Visible = True
                Label_2.Visible = True
                Label2.Visible = True
                Button2.Visible = True
                TextBox2.Visible = True
                Label_3.Visible = True
                Label3.Visible = True
                Button3.Visible = True
                TextBox3.Visible = True
                Label_4.Visible = True
                Label4.Visible = True
                Button4.Visible = True
                TextBox4.Visible = True
                Label_5.Visible = True
                Label5.Visible = True
                Button5.Visible = True
                TextBox5.Visible = True
                Label_6.Visible = True
                Label6.Visible = True
                Button6.Visible = True
                TextBox6.Visible = True
                Label_7.Visible = True
                Label7.Visible = True
                Button7.Visible = True
                TextBox7.Visible = True
                Label_8.Visible = True
                Label8.Visible = True
                Button8.Visible = True
                TextBox8.Visible = True
                Label_9.Visible = True
                Label9.Visible = True
                Button9.Visible = True
                TextBox9.Visible = True
                Label_10.Visible = True
                Label10.Visible = True
                Button10.Visible = True
                TextBox10.Visible = True
                Label_11.Visible = True
                Label11.Visible = True
                Button11.Visible = True
                TextBox11.Visible = True
                Label_12.Visible = True
                Label12.Visible = True
                Button12.Visible = True
                TextBox12.Visible = True
                Label_13.Visible = True
                Label13.Visible = True
                Button13.Visible = True
                TextBox13.Visible = True
                Label_14.Visible = True
                Label14.Visible = True
                Button14.Visible = True
                TextBox14.Visible = True
                Label_15.Visible = True
                Label15.Visible = True
                Button15.Visible = True
                TextBox15.Visible = True
                Label_16.Visible = True
                Label16.Visible = True
                Button16.Visible = True
                TextBox16.Visible = True
                Label_17.Visible = True
                Label17.Visible = True
                Button17.Visible = True
                TextBox17.Visible = True
                Label_18.Visible = True
                Label18.Visible = True
                Button18.Visible = True
                TextBox18.Visible = True
                Label_19.Visible = True
                Label19.Visible = True
                Button19.Visible = True
                TextBox19.Visible = True
                Label_20.Visible = True
                Label20.Visible = True
                Button20.Visible = True
                ListBox2.Height = 400
            Case Else
                MsgBox("Solo hay espacio para 20 lineas de codigo", vbCritical, "Error Fatal")
        End Select
    End Sub

    Private Sub FuTodo(xx)
        Dim j
        Dim tam As Integer
        tam = Len(Text1.Text)
        Dim letra As String
        Dim i, h, r As String
        Dim pri, printf As String

        r = 1
        j = 0
        h = 0
        i = 1

        If Mid(ListBox1.Items(xx), i, 1) = "#" Then
            i = 2
            If Mid(ListBox1.Items(xx), i, 7) = "INCLUDE" Then
                i = i + 7
                If Mid(ListBox1.Items(xx), i, 1) = "<" Then
                    i = i + 1
                    If (Mid(ListBox1.Items(xx), i, 5) = "STDIO") Or (Mid(ListBox1.Items(xx), i, 5) = "CONIO") Or (Mid(ListBox1.Items(xx), i, 6) = "STDLIB") Or (Mid(ListBox1.Items(xx), i, 6) = "STRING") Or (Mid(ListBox1.Items(xx), i, 6) = "DOS") Then
                        i = i + 5
                        If Mid(ListBox1.Items(xx), i, 1) = "." Then
                            i = i + 1
                            If Mid(ListBox1.Items(xx), i, 1) = "H" Then
                                i = i + 1
                                If Mid(ListBox1.Items(xx), i, 1) = ">" Then
                                    ListBox2.ForeColor = Color.Blue
                                    ListBox2.Items.Add(" LIBRERIA CORRECTA ")
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR FIN DE LIBRERIA INVALIDO POSICION ( > ) ")
                                End If
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR LIBRERIA SIN TERMINACION POSICION < H > ")
                            End If
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR LIBRERIA SIN ESPECIFICADOR POSICION < . > ")
                        End If
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR PALABRA RESERVADA NO RECONOCIDA POSICION <  > ")
                    End If
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INICIO DE LIBRERIA INVALIDO POSICION ( < ) ")
                End If
            End If


        ElseIf Mid(ListBox1.Items(xx), i, 1) = "V" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 3) = "OID" Then
                i = i + 3
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    If Mid(ListBox1.Items(xx), i, 4) = "MAIN" Then
                        i = i + 4
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                                i = i + 1
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                        i = i + 1
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                            i = i + 1
                                            If Mid(ListBox1.Items(xx), i, 1) = "{" Then
                                                i = i + 1
                                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                    i = i + 1
                                                    If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                                        ListBox2.ForeColor = Color.Blue
                                                        ListBox2.Items.Add(" VOID MAIN VALIDO ")
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR FIN DE MAIN INESPERADO POSICION ( } ) ")
                                                    End If
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                                                End If
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR FALTA COMIENZO DEL MAIN POSICION < { > ")
                                            End If
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                                        End If
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR PARAMETRO INVALIDO < ) > ")
                                    End If
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                                    '12,1
                                End If
                            End If
                        End If
                    End If
                End If
            End If


        ElseIf Mid(ListBox1.Items(xx), i, 1) = "P" Then
            i = i + 1
            '2
            If Mid(ListBox1.Items(xx), i, 5) = "RINTF" Then
                i = i + 5
                '3
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    '4
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        '5
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            '6
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(34) Then
                                i = i + 1
                                '7
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    r = 1
                                    While r = 1
                                        If (Mid(ListBox1.Items(xx), i, 1)) <> Chr(34) And (Mid(ListBox1.Items(xx), i, 1) <> ")") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                                            letra = Mid(ListBox1.Items(xx), i, 1)
                                            cadena = cadena + letra
                                            i = i + 1
                                            r = 1
                                        Else
                                            r = 0
                                        End If
                                    End While

                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(34) Then
                                        i = i + 1
                                        '9
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                            i = i + 1
                                            '10
                                            If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                                i = i + 1
                                                '11
                                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                    i = i + 1
                                                    '12
                                                    If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                        ListBox2.Items.Add(" PRINTF VALIDO")
                                                        ' pri = InputBox(cadena, "Printf", printf)
                                                        'pri = InputBox(cadena, "Printf")

                                                        MsgBox(cadena)
                                                        cadena = ""
                                                        'MsgBox(mensaje)
                                                        ListBox2.ForeColor = Color.Blue
                                                        '12
                                                    Else
                                                        ListBox2.Items.Add("FIN DEL PRINTF INESPERADO ERROR EN LA POSICION < ; >")
                                                        ListBox2.ForeColor = Color.Red
                                                    End If
                                                    'ListBox2.Items.Add("FIN DEL PRINTF INESPERADO ERROR EN LA POSICION < >")
                                                    'ListBox2.ForeColor = Color.Red
                                                End If
                                            ElseIf Mid(ListBox1.Items(xx), i, 1) = Chr(44) Then
                                                i = i + 1
                                                r = 1
                                                While r = 1
                                                    If (Mid(ListBox1.Items(xx), i, 1) <> ")") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                                                        letra = Mid(ListBox1.Items(xx), i, 1)
                                                        cadena2 = cadena2 + letra
                                                        i = i + 1 : r = 1
                                                    Else
                                                        r = 0
                                                    End If
                                                End While
                                                If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                                    i =
                                                    i + 1
                                                    '11
                                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                        i = i + 1
                                                        '12,1
                                                        If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                            ListBox2.Items.Add(" PRINTF CON ESPECIFICADOR VALIDO  ")
                                                            'pri = InputBox(cadena, "Printf")
                                                            'MsgBox(cadena)
                                                                MsgBox(cadena & mensaje)
                                                                cadena = ""

                                                                ListBox2.ForeColor = Color.Blue
                                                            Else
                                                                ListBox2.Items.Add("FIN DEL PRINTF INESPERADO ERROR EN LA POSICION <;>")
                                                            ListBox2.ForeColor = Color.Red
                                                            End If
                                                            '11,1
                                                        Else
                                                            ListBox2.Items.Add("FIN DEL PRINTF INESPERADO ERROR EN LA POSICION < >")
                                                            ListBox2.ForeColor = Color.Red
                                                        End If
                                                        '10,1
                                                    Else
                                                        ListBox2.Items.Add("FIN DEL PRINTF INESPERADO ERROR EN LA POSICION <) >")
                                                        ListBox2.ForeColor = Color.Red
                                                    End If
                                            Else
                                                ListBox2.Items.Add("FIN DEL LINEA INESPERADO ERROR EN LA POSICION <) >")
                                                ListBox2.ForeColor = Color.Red
                                            End If
                                            '9
                                        Else
                                            ListBox2.Items.Add(" ERROR AL GUARDAR CADENA  POSICION <  >")
                                            ListBox2.ForeColor = Color.Red
                                        End If
                                        '8
                                    Else
                                        ListBox2.Items.Add(" ERROR AL FINALIZAR LA ESCRITURA POSICION <  >")
                                        ListBox2.ForeColor = Color.Red
                                    End If
                                    '7
                                Else
                                    ListBox2.Items.Add(" ERROR AL ESCRIBIR  POSICION <  >")
                                    ListBox2.ForeColor = Color.Red
                                End If
                                '6
                            Else
                                ListBox2.Items.Add(" ERROR AL INICIAR LA ESCRITURA POSICION <  > ")
                                ListBox2.ForeColor = Color.Red
                            End If
                            '5
                        Else
                            ListBox2.Items.Add(" ERROR AL RESERVAR ESPACIO PARA LA CADENA  POSICION <  >")
                            ListBox2.ForeColor = Color.Red
                        End If
                        '4
                    Else
                        ListBox2.Items.Add(" ERROR AL INICIAR EL PRINTF < ( >")
                        ListBox2.ForeColor = Color.Red
                    End If
                    '3
                Else
                    ListBox2.Items.Add(" ESPACIO DE CADENA INVALIDO  POSICION <  >")
                    ListBox2.ForeColor = Color.Red
                End If
                '2
            Else
                ListBox2.Items.Add(" PALABRA RESERVADA INVALIDA <PRINTF>")
                ListBox2.ForeColor = Color.Red
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 1) = "S" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 4) = "CANF" Then
                i = i + 4
                '2
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    '3
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        '4
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            '5
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(34) Then
                                i = i + 1
                                r = 1
                                'while y if fuera de todo
                                While r = 1
                                    '5,1
                                    If (Mid(ListBox1.Items(xx), i, 1)) <> Chr(34) And (Mid(ListBox1.Items(xx), i, 1) <> ")") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                                        letra = Mid(ListBox1.Items(xx), i, 1)
                                        cadena = cadena + letra
                                        i = i + 1
                                        r = 1
                                        '5,1
                                    Else
                                        r = 0
                                    End If
                                End While
                                '6
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(34) Then
                                    i = i + 1
                                    '7
                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                        i = i + 1
                                        '8
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(44) Then
                                            i = i + 1
                                            '9
                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                i = i + 1
                                                r = 1
                                                'while y if fuera de todo
                                                While r = 1
                                                    '9,1
                                                    If (Mid(ListBox1.Items(xx), i, 1)) <> Chr(34) And (Mid(ListBox1.Items(xx), i, 1) <> ")") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                                                        letra = Mid(ListBox1.Items(xx), i, 1)
                                                        cadena2 = cadena2 + letra
                                                        i = i + 1
                                                        r = 1
                                                        '9,1
                                                    Else
                                                        r = 0
                                                    End If
                                                End While
                                                '10
                                                If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                                    i = i + 1
                                                    '11
                                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                        i = i + 1
                                                        '11
                                                        If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                            i = i + 1
                                                            ListBox2.ForeColor = Color.Blue
                                                            ListBox2.Items.Add(" SCANF VALIDO CHECAR ESPECIFICADORES Y VARIABLES ALMACENADAS ")
                                                            'MsgBox("ESPECIFICADORES        " & cadena)
                                                            'MsgBox("VARIABLES              " & cadena2)
                                                            mensaje = InputBox(alm)
                                                            cadena = ""
                                                            '12
                                                        Else
                                                            ListBox2.ForeColor = Color.Red
                                                            ListBox2.Items.Add(" ERROR FIN DE SCANF INESPERADO POSICION < ; > ")
                                                        End If
                                                        '11
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                                    End If
                                                    '10
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR FIN DE LECTURA INVALIDO POSICION < ) > ")
                                                End If
                                                '9
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                            End If
                                            '8
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR OPERADOR NO ENCONTRADO  POSICION < , > ")
                                        End If
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA  POSICION <   > ")
                                    End If
                                    '6
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR FIN DE ESPECIFICADORES INESPERADO POSICION <  > ")
                                End If
                                '5
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR INICIO DE ESPECIFICADORES INVALIDO POSICION <  > ")
                            End If
                            '4
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                        End If
                        '3
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR INCIO DE LECTURA INVALIDON POSICION < ( > ")
                    End If
                    '2
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCION INVALIDA POSICION <  > ")
                End If
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERVADA INCORRECTA POSICION <SCANF> ")
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 3) = "INT" Then
            i = i + 3
            '2
            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                i = i + 1
                r = 1
                'while y if fuera de todo
                While r = 1
                    If (Mid(ListBox1.Items(xx), i, 1)) <> "[" And (Mid(ListBox1.Items(xx), i, 1) <> "]") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                        letra = Mid(ListBox1.Items(xx), i, 1)
                        cadena = cadena + letra
                        i = i + 1
                        r = 1
                    Else
                        r = 0
                    End If
                End While
                '3,1
                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                    ListBox2.ForeColor = Color.Blue
                    ListBox2.Items.Add(" INT CORRECTO,     INT NORMAL  ")
                    '3,2
                ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                    i = i + 1
                    '3,2,1
                    If Mid(
                    ListBox1.Items(xx), i, 1) = Chr(32) Then
                        i = i + 1
                        '3,2,1,1
                        If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                            i = i + 1
                            '3,2,1,1,1
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                i = i + 1
                                '3,2,4
                                If Mid(
                                ListBox1.Items(xx), i, 1) = "]" Then
                                    i = i + 1
                                    '3,2,5
                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                        i = i + 1
                                        '3,2,6
                                        If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                            ListBox2.ForeColor = Color.Blue
                                            ListBox2.Items.Add(" INT CORRECTO,     INT SIMPLE CON ARREGLOS ")
                                            '3,2,6,1
                                        ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                                            i = i + 1
                                            '3,2,1
                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                i = i + 1
                                                '3,2,1,1
                                                If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                                                    i = i + 1
                                                    '3,2,1,1,1
                                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                        i = i + 1
                                                        '3,2,4
                                                        If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                                            i = i + 1
                                                            '3,2,5
                                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                                i = i + 1
                                                                '3,2,6
                                                                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                                    ListBox2.ForeColor = Color.Blue
                                                                    ListBox2.Items.Add(" INT CORRECTO,     INT COMPUESTO CON MATRICES  ")
                                                                End If
                                                                '3,2,6
                                                            Else
                                                                ListBox2.ForeColor = Color.Red
                                                                ListBox2.Items.Add(" ERROR FIN DE INT INESPERADO POSICION < ; > ")
                                                            End If
                                                            '3,2,5
                                                        Else
                                                            ListBox2.ForeColor = Color.Red
                                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                                        End If
                                                        '3,2,4
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR FIN DE MATRIZ INESPERADO POSICION < ] > ")
                                                    End If
                                                    '3,2,1,1,1
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                                End If
                                                '3,2,1,1
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <1,...9> ")
                                            End If
                                            '3,2,1
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION ; ")
                                        End If
                                        '3,2,6,3
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                    End If
                                    '3,2,5
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INICIO DE MATRIZ INVALIDA POSICION < ] > ")
                                End If
                                '3,2,4
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                            End If
                            '3,2,1,1,1
                        Else
                        End If
                        '3,2,1,1
                    Else
                    End If
                    '3,2,1
                Else
                End If
                '3,3
            Else
            End If
            '2


        ElseIf Mid(ListBox1.Items(xx), i, 1) = "c" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 5) = "lrscr" Then
                i = i + 5
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                i = i + 1
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                        ListBox2.ForeColor = Color.Blue
                                        ListBox2.Items.Add(" BORRADO DE PANTALLA VALIDO ")
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE CLRSCR INESPERADO POSICION < ; > ")
                                    End If
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                                End If
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < ) > ")
                            End If
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                        End If
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < ( > ")
                    End If
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                End If
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERVADA NO RECONOCIDA POSICION <CLRSCR> ")
            End If

        

        ElseIf Mid(ListBox1.Items(xx), i, 2) = "pu" Then
            i = i + 2
            '2
            If Mid(ListBox1.Items(xx), i, 2) = "ts" Then
                i = i + 2
                '3
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    '4
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        '5
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            h = i - 1
                            '6
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(34) Then
                                i = i + 1
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    r = 1
                                    'while y if fuera de todo
                                    While r = 1
                                        '7,1
                                        If (Mid(ListBox1.Items(xx), i, 1)) <> Chr(34) And (Mid(ListBox1.Items(xx), i, 1) <> ")") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                                            letra = Mid(ListBox1.Items(xx), i, 1)
                                            cadena = cadena + letra
                                            i = i + 1
                                            r = 1
                                            '7,1
                                        Else : r = 0
                                        End If
                                    End While
                                    '8
                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(34) Then
                                        i = i + 1
                                        '9
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                            i = i + 1
                                            '10
                                            If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                                i = i + 1
                                                '11
                                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                    i = i + 1
                                                    '12
                                                    If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                        ListBox2.ForeColor = Color.Blue
                                                        ListBox2.Items.Add(" PUTS CORRECTO --DESPLEGASTE UN LETRERO")
                                                        '  pri = InputBox(cadena, "PUTS", printf)
                                                        MsgBox(cadena)
                                                        cadena = ""
                                                        '12
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR FIN DE PUTS INESPERADO POSICION < ; > ")
                                                    End If
                                                    '11
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                End If
                                                '10
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR FIN DE CADENA INVALIDO < ) > ")
                                            End If
                                            '9
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '8
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE MENSAJE INVALIDO <  > ")
                                    End If
                                    '7
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                ' 6 cadena ok
                            ElseIf Mid(ListBox1.Items(xx), h, 1) = Chr(32) Then
                                r = 1
                                'while y if fuera de todo
                                While r = 1
                                    '6,1,1
                                    If (Mid(ListBox1.Items(xx), h, 1) <> ")") And (Mid(ListBox1.Items(xx), h, 1) <> ";") Then
                                        letra = Mid(ListBox1.Items(xx), h, 1)
                                        cadena2 = cadena2 +
                                        letra
                                        h = h + 1 : r = 1
                                        '6,1,1
                                    Else : r = 0
                                    End If
                                End While
                                '7
                                If Mid(ListBox1.Items(xx), h, 1) = ")" Then
                                    h = h + 1
                                    '8
                                    If Mid(ListBox1.Items(xx), h, 1) = Chr(32) Then
                                        h = h + 1
                                        '9
                                        If Mid(ListBox1.Items(xx), h, 1) = ";" Then
                                            ListBox2.ForeColor = Color.Blue
                                            ListBox2.Items.Add(" PUTS CORRECTO --DESPLEGASTE EL VALOR DE UNA VARIABLE")
                                            MsgBox(cadena2)
                                            '9
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR FIN DE PUTS INESPERADO POSICION < ; > ")
                                        End If
                                        '8
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                    End If
                                    '7
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR FIN DE CADENA INVALIDO < ) > ")
                                End If
                                '6
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add("ERROR INICIO DE MENSAJE INVALIDO < " > "")
                            End If
                            '5
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                        End If
                        '4
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR FIN DE CADENAINVALIDO < ( > ")
                    End If
                    '3
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                End If
                '2
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" PALABRA RESERVADA INVALIDA <PUTS> ")
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 1) = "G" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 4) = "ETCH" Then
                i = i + 4
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        If Mid(ListBox1.Items(xx), i, 1) =
                        Chr(32) Then
                            i = i + 1
                            If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                i = i + 1
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                        ListBox2.Items.Add(" PÂUSA DE PROGRAMA VALIDO ")
                                        ListBox2.ForeColor = Color.Blue
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE GETCH INESPERADO POSICION < ; > ")
                                    End If
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                                End If
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < ) > ")
                            End If
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                        End If
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < ( > ")
                    End If
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                End If
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERBADA NO RECONOCIDA POSICION <GETCH> ")
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 1) = "J" Then
            i = i + 1
            '1
            If Mid(ListBox1.Items(xx), i, 3) = "ETS" Then
                i = i + 3
                '2
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    '3
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        '4
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            r = 1
                            'while y if fuera de todo
                            While r = 1
                                '7,1
                                If (Mid(ListBox1.Items(xx), i, 1)) <> Chr(34) And (Mid(ListBox1.Items(xx), i, 1) <> ")") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                                    letra = Mid(ListBox1.Items(xx), i, 1)
                                    cadena = cadena + letra
                                    i = i + 1
                                    r = 1
                                    '7,1
                                Else
                                    r = 0
                                End If
                            End While
                            '5
                            If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                i = i + 1
                                '6
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    '7
                                    If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                        ListBox2.ForeColor = Color.Blue
                                        ListBox2.Items.Add(" JETS CORRECTO LEISTE EL VALOR DE UNA VARIABLE")
                                        'MsgBox(cadena)
                                        MsgBox(mensaje)
                                        cadena = ""
                                        '7
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE GETS INESPERADO POSICION < ; > ")
                                    End If
                                    '6
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '5
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR AL FINALIZAR LA LECTURA POSICION < ) > ")
                            End If
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                        End If
                        '3
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR AL INICIAR LA LECTURA POSICION < ( > ")
                    End If
                    '2
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                End If
                '1
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" PALABRA RESERVADA NO RECONOCIDA <JETS  > ")
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 1) = "I" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 1) = "F" Then
                i = i + 1
                '2
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    '3
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        '4
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            r = 1
                            'while y if fuera de todo
                            While r = 1
                                '7,1
                                If (Mid(ListBox1.Items(xx), i, 1)) <> "<" And (Mid(ListBox1.Items(xx), i, 1) <> ">") And (Mid(ListBox1.Items(xx), i, 1) <> "=") Then
                                    letra = Mid(ListBox1.Items(xx), i, 1)
                                    cadena = cadena + letra
                                    i = i + 1
                                    r = 1
                                    '7,1
                                Else
                                    r = 0
                                End If
                            End While
                            '5
                            If (Mid(ListBox1.Items(xx), i, 2) = "<=") Or (Mid(ListBox1.Items(xx), i, 2) = ">=") Or (Mid(ListBox1.Items(xx), i, 2) = "<>") Or (Mid(ListBox1.Items(xx), i, 2) = "==") Then
                                i = i + 2
                                '6
                                '6
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    r = 1
                                    'while y if fuera de todo
                                    While r = 1
                                        '7,1
                                        If (Mid(ListBox1.Items(xx), i, 1)) <> ")" And (Mid(ListBox1.Items(xx), i, 1) <> "{") And (Mid(ListBox1.Items(xx), i, 1) <> "}") Then
                                            letra = Mid(ListBox1.Items(xx), i, 1)
                                            cadena = cadena + letra
                                            i = i + 1 : r = 1
                                            '7,1
                                        Else : r = 0
                                        End If
                                    End While
                                    '7
                                    If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                        i = i + 1
                                        '8
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                            i = i + 1
                                            '9
                                            If Mid(ListBox1.Items(xx), i, 1) = "{" Then
                                                i = i + 1
                                                '10
                                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                    i = i + 1
                                                    '11
                                                    If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                                        ListBox2.ForeColor = Color.Blue
                                                        ListBox2.Items.Add(" IF CORRECTO -- COMPARACION VALIDA")
                                                        cadena = ""
                                                        '11
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR FIN DE IF INESPERADO POSICION < } > ")
                                                    End If
                                                    '10
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                                End If
                                                '9
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INICIO DE IF INESPERADO POSICION < { > ")
                                            End If
                                            '8
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '7
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE COMPARACION INVALIDO POSICION < ) > ")
                                    End If
                                    '6
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '5 OTRO MAS
                            ElseIf (Mid(ListBox1.Items(xx), i, 1) = "<") Or (Mid(ListBox1.Items(xx), i, 1) = ">") Then
                                i = i + 1
                                '6
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    r = 1
                                    'while y if fuera de todo
                                    While r = 1
                                        '7,1
                                        If (Mid(ListBox1.Items(xx), i, 1)) <> ")" And (Mid(ListBox1.Items(xx), i, 1) <> "{") And (Mid(ListBox1.Items(xx), i, 1) <> "}") Then
                                            letra = Mid(ListBox1.Items(xx), i, 1)
                                            cadena = cadena + letra
                                            i = i + 1 : r = 1
                                            '7,1
                                        Else : r = 0
                                        End If
                                    End While
                                    '7
                                    If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                        i = i + 1
                                        '8
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                            i = i + 1
                                            '9
                                            If Mid(ListBox1.Items(xx), i, 1) = "{" Then
                                                i = i + 1
                                                '10
                                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                    i = i +
                                                    1
                                                    '11
                                                    If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                                        ListBox2.ForeColor = Color.Blue
                                                        ListBox2.Items.Add(" IF CORRECTO --COMPARACION VALIDA")
                                                        cadena = ""
                                                        '11
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR FIN DE IF INESPERADO POSICION < } > ")
                                                    End If
                                                    '10
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                                End If
                                                '9
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INICIO DE IF INESPERADO POSICION < { > ")
                                            End If
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '7
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE COMPARACION INVALIDO POSICION <  > ")
                                    End If
                                    '6
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '5
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION ( >,<,<=,<=) ")
                            End If
                            '4
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                        End If
                        '3
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR INICIO DE COMPARACION INVALIDA POSICION < ( > ")
                    End If
                    '2
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                End If
                '1
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERVADA INCORRECTA <IF,INT> ")
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 5) = "FLOAT" Then
            i = i + 5
            '2
            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                i = i + 1
                r = 1
                'while y if fuera de todo
                While r = 1
                    If (Mid(ListBox1.Items(xx), i, 1)) <> "[" And (Mid(ListBox1.Items(xx), i, 1) <> "]") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                        letra = Mid(ListBox1.Items(xx), i, 1)
                        cadena = cadena + letra
                        i = i + 1
                        r = 1
                    Else
                        r = 0
                    End If
                End While
                '3,1
                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                    ListBox2.ForeColor = Color.Blue
                    ListBox2.Items.Add(" FLOAT CORRECTO,     FLOAT NORMAL  ")
                    '3,2
                ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                    i = i + 1
                    '3,2,1
                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                        i = i + 1
                        '3,2,1,1
                        If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                            i = i + 1
                            '3,2,1,1,1
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                i = i + 1
                                '3,2,4
                                If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                    i = i + 1
                                    '3
                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                        i = i + 1
                                        '3,2,6
                                        If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                            ListBox2.ForeColor = Color.Blue
                                            ListBox2.Items.Add(" FLOAT CORRECTO,     FLOAT SIMPLE CON ARREGLOS  ")
                                            '3,2,6,1
                                        ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                                            i = i + 1
                                            '3,2,1
                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                i = i + 1
                                                '3,2,1,1
                                                If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                                                    i = i + 1
                                                    '3,2,1,1,1
                                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                        i = i + 1
                                                        '3,2,4
                                                        If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                                            i = i + 1
                                                            '3,2,5
                                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                                i = i + 1
                                                                '3,2,6
                                                                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                                    ListBox2.ForeColor = Color.Blue
                                                                    ListBox2.Items.Add(" FLOAT CORRECTO,     FLOAT COMPUESTO CON MATRICES  ")
                                                                    '3,2,6
                                                                Else
                                                                    ListBox2.ForeColor = Color.Red
                                                                    ListBox2.Items.Add(" ERROR FIN DE FLOAT INESPERADO POSICION < ; > ")
                                                                End If
                                                                '3,2,5
                                                            Else
                                                                ListBox2.ForeColor = Color.Red
                                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                            End If
                                                            '3,2,4
                                                        Else
                                                            ListBox2.ForeColor = Color.Red
                                                            ListBox2.Items.Add(" ERROR FIN DE MATRIZ INESPERADO POSICION < ] > ")
                                                        End If
                                                        '3,2,1,1,1
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                    End If
                                                    '3,2,1,1
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <1,...9> ")
                                                End If
                                                '3,2,1
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                            End If
                                            '3,2,6,3
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '3,2,5
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR INICIO DE MATRIZ INVALIDA POSICION < [ > ")
                                    End If
                                    '3,2,4
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '3,2,1,1,1
                            Else
                            End If
                            '3,2,1,1
                        Else
                        End If
                        '3,2,1
                    Else
                    End If
                    '3,3
                Else
                End If
                '2
            Else
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 4) = "CHAR" Then
            i = i + 4
            '2
            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                i = i + 1
                r = 1
                'while y if fuera de todo
                While r = 1
                    If (Mid(ListBox1.Items(xx), i, 1)) <> "[" And (Mid(ListBox1.Items(xx), i, 1) <> "]") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                        letra = Mid(ListBox1.Items(xx), i, 1)
                        cadena = cadena + letra
                        i = i + 1
                        r = 1
                    Else
                        r = 0
                    End If
                End While
                '3,1
                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                    ListBox2.ForeColor = Color.Blue
                    ListBox2.Items.Add(" CHAR CORRECTO,     CHAR NORMAL  ")
                    '3,2
                ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                    i = i + 1
                    '3,2,1
                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                        i = i + 1
                        '3,2,1,1
                        If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                            i = i + 1
                            '3,2,1,1,1
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                i = i + 1
                                '3,2,4
                                If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                    i = i + 1
                                    '3
                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                        i = i + 1
                                        '3,2,6
                                        If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                            ListBox2.ForeColor = Color.Blue
                                            ListBox2.Items.Add(" CHAR CORRECTO,     CHAR SIMPLE CON ARREGLOS  ")
                                            '3,2,6,1
                                        ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                                            i = i + 1
                                            '3,2,1
                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                i = i + 1
                                                '3,2,1,1
                                                If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                                                    i = i + 1
                                                    '3,2,1,1,1
                                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                        i = i + 1
                                                        '3,2,4
                                                        If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                                            i = i + 1
                                                            '3,2,5
                                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                                i = i + 1
                                                                '3,2,6
                                                                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                                    ListBox2.ForeColor = Color.Blue
                                                                    ListBox2.Items.Add(" CHAR CORRECTO,     CHAR COMPUESTO CON MATRICES  ")
                                                                    '3,2,6
                                                                Else
                                                                    ListBox2.ForeColor = Color.Red
                                                                    ListBox2.Items.Add(" ERROR FIN DE CHAR INESPERADO POSICION < ; > ")
                                                                End If
                                                                '3,2,5
                                                            Else
                                                                ListBox2.ForeColor = Color.Red
                                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                            End If
                                                            '3,2,4
                                                        Else
                                                            ListBox2.ForeColor = Color.Red
                                                            ListBox2.Items.Add(" ERROR FIN DE MATRIZ INESPERADO POSICION < ] > ")
                                                        End If
                                                        '3,2,1,1,1
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                    End If
                                                    '3,2,1,1
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <1,...9> ")
                                                End If
                                                '3,2,1
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                            End If
                                            '3,2,6,3
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '3,2,5
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR INICIO DE MATRIZ INVALIDA POSICION < ; > ")
                                    End If
                                    '3,2,4
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '3,2,1,1,1
                            Else
                            End If
                            '3,2,1,1
                        Else
                        End If
                        '3,2,1
                    Else
                    End If
                    '3,3
                Else
                End If
                '2
            Else
            End If

            '1
        ElseIf Mid(ListBox1.Items(xx), i, 1) = "W" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 4) = "HILE" Then
                i = i + 4
                '2
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    '3
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        '4
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            r = 1
                            'while y if fuera de todo
                            While r = 1
                                '7,1
                                If (Mid(ListBox1.Items(xx), i, 1)) <> "<" And (Mid(ListBox1.Items(xx), i, 1) <> ">") And (Mid(ListBox1.Items(xx), i, 1) <> "=") Then
                                    letra = Mid(ListBox1.Items(xx), i, 1)
                                    cadena = cadena + letra
                                    i = i + 1
                                    r = 1
                                    '7,1
                                Else
                                    r = 0
                                End If
                            End While
                            '5
                            If (Mid(ListBox1.Items(xx), i, 2) = "<=") Or (Mid(ListBox1.Items(xx), i, 2) = ">=") Or (Mid(ListBox1.Items(xx), i, 2) = "<>") Or (Mid(ListBox1.Items(xx), i, 2) = "==") Then
                                i = i + 2
                                '6
                                '6
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    r = 1
                                    'while y if fuera de todo
                                    While r = 1
                                        '7,1
                                        If (Mid(ListBox1.Items(xx), i, 1)) <> ")" And (Mid(ListBox1.Items(xx), i, 1) <> "{") And (Mid(ListBox1.Items(xx), i, 1) <> "}") Then
                                            letra = Mid(ListBox1.Items(xx), i, 1)
                                            cadena = cadena + letra
                                            i = i + 1 : r = 1
                                            '7,1
                                        Else : r = 0
                                        End If
                                    End While
                                    '7
                                    If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                        i = i + 1
                                        '8
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                            i = i + 1
                                            '9
                                            If Mid(ListBox1.Items(xx), i, 1) = "{" Then
                                                i = i + 1
                                                '10
                                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                    i = i + 1
                                                    '11
                                                    If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                                        ListBox2.ForeColor = Color.Blue
                                                        ListBox2.Items.Add(" WHILE CORRECTO -- COMPARACION VALIDA")
                                                        cadena = ""
                                                        '11
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR FIN DE WHILE INESPERADO POSICION < } > ")
                                                    End If
                                                    '10
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION < > ")
                                                End If
                                                '9
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INICIO DE WHILE INESPERADO POSICION < { > ")
                                            End If
                                            '8
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '7
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE COMPARACION INVALIDO POSICION < ) > ")
                                    End If
                                    '6
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '5 OTRO MAS
                            ElseIf (Mid(ListBox1.Items(xx), i, 1) = "<") Or (Mid(ListBox1.Items(xx), i, 1) = ">") Then
                                i = i + 1
                                '6
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    r = 1
                                    'while y if fuera de todo
                                    While r = 1
                                        '7,1
                                        If (Mid(ListBox1.Items(xx), i, 1)) <> ")" And (Mid(ListBox1.Items(xx), i, 1) <> "{") And (Mid(ListBox1.Items(xx), i, 1) <> "}") Then
                                            letra = Mid(ListBox1.Items(xx), i, 1)
                                            cadena = cadena + letra
                                            i = i + 1 : r = 1
                                            '7,1
                                        Else : r = 0
                                        End If
                                    End While
                                    '7
                                    If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                        i = i + 1
                                        '8
                                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                            i = i + 1
                                            '9
                                            If Mid(ListBox1.Items(xx), i, 1) = "{" Then
                                                i = i + 1
                                                '10
                                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                    i = i +
                                                    1
                                                    '11
                                                    If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                                        ListBox2.ForeColor = Color.Blue
                                                        ListBox2.Items.Add(" WHILE CORRECTO --COMPARACION VALIDA")
                                                        cadena = ""
                                                        '11
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR FIN DE WHILE INESPERADO POSICION < } > ")
                                                    End If
                                                    '10
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION < > ")
                                                End If
                                                '9
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INICIO DE WHILE INESPERADO POSICION < { > ")
                                            End If
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '7
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE COMPARACION INVALIDO POSICION <  > ")
                                    End If
                                    '6
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '5
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION ( >,<,<=,<=) ")
                            End If
                            '4
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                        End If
                        '3
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR INICIO DE COMPARACION INVALIDA POSICION < ( > ")
                    End If
                    '2
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                End If
                '1
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERVADA INCORRECTA <WHILE,INT> ")
            End If

        ElseIf Mid(ListBox1.Items(xx), i, 6) = "DOUBLE" Then
            i = i + 6
            '2
            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                i = i + 1
                r = 1
                'while y if fuera de todo
                While r = 1
                    If (Mid(ListBox1.Items(xx), i, 1)) <> "[" And (Mid(ListBox1.Items(xx), i, 1) <> "]") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                        letra = Mid(ListBox1.Items(xx), i, 1)
                        cadena = cadena + letra
                        i = i + 1
                        r = 1
                    Else
                        r = 0
                    End If
                End While
                '3,1
                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                    ListBox2.ForeColor = Color.Blue
                    ListBox2.Items.Add(" DOUBLE CORRECTO,     DOUBLE NORMAL  ")
                    '3,2
                ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                    i = i + 1
                    '3,2,1
                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                        i = i + 1
                        '3,2,1,1
                        If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                            i = i + 1
                            '3,2,1,1,1
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                i = i + 1
                                '3,2,4
                                If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                    i = i + 1
                                    '3
                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                        i = i + 1
                                        '3,2,6
                                        If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                            ListBox2.ForeColor = Color.Blue
                                            ListBox2.Items.Add(" DOUBLE CORRECTO,     DOUBLE SIMPLE CON ARREGLOS  ")
                                            '3,2,6,1
                                        ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                                            i = i + 1
                                            '3,2,1
                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                i = i + 1
                                                '3,2,1,1
                                                If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                                                    i = i + 1
                                                    '3,2,1,1,1
                                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                        i = i + 1
                                                        '3,2,4
                                                        If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                                            i = i + 1
                                                            '3,2,5
                                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                                i = i + 1
                                                                '3,2,6
                                                                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                                    ListBox2.ForeColor = Color.Blue
                                                                    ListBox2.Items.Add(" DOUBLE CORRECTO,     DOUBLE COMPUESTO CON MATRICES  ")
                                                                    '3,2,6
                                                                Else
                                                                    ListBox2.ForeColor = Color.Red
                                                                    ListBox2.Items.Add(" ERROR FIN DE DOUBLE INESPERADO POSICION < ; > ")
                                                                End If
                                                                '3,2,5
                                                            Else
                                                                ListBox2.ForeColor = Color.Red
                                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                            End If
                                                            '3,2,4
                                                        Else
                                                            ListBox2.ForeColor = Color.Red
                                                            ListBox2.Items.Add(" ERROR FIN DE MATRIZ INESPERADO POSICION < ] > ")
                                                        End If
                                                        '3,2,1,1,1
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                    End If
                                                    '3,2,1,1
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <1,...9> ")
                                                End If
                                                '3,2,1
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                            End If
                                            '3,2,6,3
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION < [ > ")
                                        End If
                                        '3,2,5
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR INICIO DE MATRIZ INVALIDA POSICION <  > ")
                                    End If
                                    '3,2,4
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION < ] > ")
                                End If
                                '3,2,1,1,1
                            Else
                            End If
                            '3,2,1,1
                        Else
                        End If
                        '3,2,1
                    Else
                    End If
                    '3,3
                Else
                End If
                '2
            Else
            End If


        ElseIf Mid(ListBox1.Items(xx), i, 4) = "LONG" Then
            i = i + 4
            '2
            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                i = i + 1
                r = 1
                'while y if fuera de todo
                While r = 1
                    If (Mid(ListBox1.Items(xx), i, 1)) <> "[" And (Mid(ListBox1.Items(xx), i, 1) <> "]") And (Mid(ListBox1.Items(xx), i, 1) <> ";") Then
                        letra = Mid(ListBox1.Items(xx), i, 1)
                        cadena = cadena + letra
                        i = i + 1
                        r = 1
                    Else
                        r = 0
                    End If
                End While
                '3,1
                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                    ListBox2.ForeColor = Color.Blue
                    ListBox2.Items.Add(" LONG CORRECTO,     LONG NORMAL  ")
                    '3,2
                ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                    i = i + 1
                    '3,2,1
                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                        i = i + 1
                        '3,2,1,1
                        If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                            i = i + 1
                            '3,2,1,1,1
                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                i = i + 1
                                '3,2,4
                                If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                    i = i + 1
                                    '3
                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                        i = i + 1
                                        '3,2,6
                                        If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                            ListBox2.ForeColor = Color.Blue
                                            ListBox2.Items.Add(" LONG CORRECTO,     LONG SIMPLE CON ARREGLOS  ")
                                            '3,2,6,1
                                        ElseIf Mid(ListBox1.Items(xx), i, 1) = "[" Then
                                            i = i + 1
                                            '3,2,1
                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                i = i + 1
                                                '3,2,1,1
                                                If Mid(ListBox1.Items(xx), i, 1) = "1" Or Mid(ListBox1.Items(xx), i, 1) = "2" Or Mid(ListBox1.Items(xx), i, 1) = "3" Or Mid(ListBox1.Items(xx), i, 1) = "4" Or Mid(ListBox1.Items(xx), i, 1) = "5" Or Mid(ListBox1.Items(xx), i, 1) = "6" Or Mid(ListBox1.Items(xx), i, 1) = "7" Or Mid(ListBox1.Items(xx), i, 1) = "8" Or Mid(ListBox1.Items(xx), i, 1) = "9" Then
                                                    i = i + 1
                                                    '3,2,1,1,1
                                                    If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                        i = i + 1
                                                        '3,2,4
                                                        If Mid(ListBox1.Items(xx), i, 1) = "]" Then
                                                            i = i + 1
                                                            '3,2,5
                                                            If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                                                i = i + 1
                                                                '3,2,6
                                                                If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                                                    ListBox2.ForeColor = Color.Blue
                                                                    ListBox2.Items.Add(" LONG CORRECTO,     LONG COMPUESTO CON MATRICES  ")
                                                                    '3,2,6
                                                                Else
                                                                    ListBox2.ForeColor = Color.Red
                                                                    ListBox2.Items.Add(" ERROR FIN DE LONG INESPERADO POSICION < ; > ")
                                                                End If
                                                                '3,2,5
                                                            Else
                                                                ListBox2.ForeColor = Color.Red
                                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                            End If
                                                            '3,2,4
                                                        Else
                                                            ListBox2.ForeColor = Color.Red
                                                            ListBox2.Items.Add(" ERROR FIN DE MATRIZ INESPERADO POSICION < ] > ")
                                                        End If
                                                        '3,2,1,1,1
                                                    Else
                                                        ListBox2.ForeColor = Color.Red
                                                        ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                                    End If
                                                    '3,2,1,1
                                                Else
                                                    ListBox2.ForeColor = Color.Red
                                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <1,...9> ")
                                                End If
                                                '3,2,1
                                            Else
                                                ListBox2.ForeColor = Color.Red
                                                ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <> ")
                                            End If
                                            '3,2,6,3
                                        Else
                                            ListBox2.ForeColor = Color.Red
                                            ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                        End If
                                        '3,2,5
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR INICIO DE MATRIZ INVALIDA POSICION < [ > ")
                                    End If
                                    '3,2,4
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION INVALIDA POSICION <  > ")
                                End If
                                '3,2,1,1,1
                            Else
                            End If
                            '3,2,1,1
                        Else
                        End If
                        '3,2,1
                    Else
                    End If
                    '3,3
                Else
                End If
                '2
            Else
            End If


        ElseIf Mid(ListBox1.Items(xx), i, 1) = "E" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 3) = "LSE" Then
                i = i + 3
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    If Mid(ListBox1.Items(xx), i, 1) = "{" Then
                        i = i + 1
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                    If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                        ListBox2.ForeColor = Color.Blue
                                        ListBox2.Items.Add(" ELSE VALIDO ")
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE ELSE INESPERADO POSICION < } > ")
                                    End If
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < } > ")
                            End If
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                        End If
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < { > ")
                    End If
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                End If
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERVADA NO RECONOCIDA POSICION <ELSE> ")
            End If


        ElseIf Mid(ListBox1.Items(xx), i, 1) = "D" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 1) = "O" Then
                i = i + 1
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    If Mid(ListBox1.Items(xx), i, 1) = "{" Then
                        i = i + 1
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                If Mid(ListBox1.Items(xx), i, 1) = "}" Then
                                    ListBox2.ForeColor = Color.Blue
                                    ListBox2.Items.Add(" DO VALIDO ")
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR FIN DE DO INESPERADO POSICION < } > ")
                                End If
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < } > ")
                            End If
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                        End If
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < { > ")
                    End If
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                End If
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERVADA NO RECONOCIDA POSICION <DO> ")
            End If


        ElseIf Mid(ListBox1.Items(xx), i, 1) = "R" Then
            i = i + 1
            If Mid(ListBox1.Items(xx), i, 5) = "ETURN" Then
                i = i + 5
                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                    i = i + 1
                    If Mid(ListBox1.Items(xx), i, 1) = "(" Then
                        i = i + 1
                        If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                            i = i + 1
                            If Mid(ListBox1.Items(xx), i, 1) = ")" Then
                                i = i + 1
                                If Mid(ListBox1.Items(xx), i, 1) = Chr(32) Then
                                    i = i + 1
                                    If Mid(ListBox1.Items(xx), i, 1) = ";" Then
                                        ListBox2.ForeColor = Color.Blue
                                        ListBox2.Items.Add(" RETURN VALIDO ")
                                    Else
                                        ListBox2.ForeColor = Color.Red
                                        ListBox2.Items.Add(" ERROR FIN DE RETURN INESPERADO POSICION < ; > ")
                                    End If
                                Else
                                    ListBox2.ForeColor = Color.Red
                                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                                End If
                            Else
                                ListBox2.ForeColor = Color.Red
                                ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < ) > ")
                            End If
                        Else
                            ListBox2.ForeColor = Color.Red
                            ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                        End If
                    Else
                        ListBox2.ForeColor = Color.Red
                        ListBox2.Items.Add(" ERROR FALTA PARAMETRO POSICION < ( > ")
                    End If
                Else
                    ListBox2.ForeColor = Color.Red
                    ListBox2.Items.Add(" ERROR INSTRUCCION NO RECONOCIDA POSICION <  > ")
                End If
            Else
                ListBox2.ForeColor = Color.Red
                ListBox2.Items.Add(" ERROR PALABRA RESERVADA NO RECONOCIDA POSICION <RETURN> ")
            End If


        End If
    End Sub

    Private Sub TextBox20_TextChanged(sender As Object, e As EventArgs)

    End Sub
End Class
