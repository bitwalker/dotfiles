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
        <ComboBox HorizontalAlignment="Left" Margin="10,30,0,0" VerticalAlignment="Top" Width="120">
            <ComboBoxItem Content="Test1" IsSelected="True"/>
            <ComboBoxItem Content="Test2"/>
            <ComboBoxItem Content="Test3"/>
            <ComboBoxItem Content="Test4"/>
        </ComboBox>
        <Button Content="Send" HorizontalAlignment="Left" Margin="10,70,0,0" VerticalAlignment="Top" Width="75"/>

    </Grid>
</Window>

"@

$reader = [System.XML.XMLReader]::Create([System.IO.StringReader] $xaml)
$window = [System.Windows.Markup.XAMLReader]::Load($reader)
$window.TopMost = $true
$window.WindowStartupLocation = 'CenterScreen'


# Fenster anzeigen
$null = $window.ShowDialog()
