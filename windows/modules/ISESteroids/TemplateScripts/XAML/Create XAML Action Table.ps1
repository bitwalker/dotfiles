# nachladen von WPF (nötig für powershell.exe, weil hier
# anders als in der ISE die WPF Bibliotheken nicht
# automatisch geladen werden:
Add-Type -AssemblyName PresentationFramework



$xaml = @"
<Window 
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    Title="MainWindow" Height="350" Width="525">
    <Grid>
        <Grid.RowDefinitions>
            <RowDefinition Height="72*"/>
            <RowDefinition Height="85*"/>
            <RowDefinition Height="162*"/>
        </Grid.RowDefinitions>
        <Grid.ColumnDefinitions>
            <ColumnDefinition Width="54*"/>
            <ColumnDefinition Width="80*"/>
            <ColumnDefinition Width="175*"/>
            <ColumnDefinition Width="208*"/>
        </Grid.ColumnDefinitions>
        <Button Name="button1" Content="Aktion1" HorizontalAlignment="Left" Height="40" Margin="0,22,0,0" VerticalAlignment="Top" Width="60" Grid.ColumnSpan="2"/>
        <Button Name="button2" Content="Machwas" Grid.Column="2" HorizontalAlignment="Left" Height="34" Margin="30,27,0,0" Grid.Row="1" VerticalAlignment="Top" Width="70"/>
        <Button Name="button3" Content="Button3" Grid.Column="3" HorizontalAlignment="Left" Height="34" Margin="108,83,0,0" VerticalAlignment="Top" Width="70" Grid.Row="2"/>

    </Grid>
</Window>

"@

$reader = [System.XML.XMLReader]::Create([System.IO.StringReader] $xaml)
$window = [System.Windows.Markup.XAMLReader]::Load($reader)
$window.TopMost = $true
$window.WindowStartupLocation = 'CenterScreen'
$button1 = $window.FindName("button1")
$code1 = {
    Write-Host "Button 1"
}
$button1.add_Click($code1)

$button2 = $window.FindName("button2")
$code2 = {
    Write-Host "Button 2"
}
$button2.add_Click($code2)

$button3 = $window.FindName("button3")
$code3 = {
    Write-Host "Button 3"
}
$button3.add_Click($code3)
# Fenster anzeigen
$null = $window.ShowDialog()
