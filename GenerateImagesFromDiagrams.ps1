$SourcePath = './diagrams.vsdx'
$DestinationDirectory = 'images'
If(Test-Path -Path $SourcePath)
{
    #get all related to Visio files
	$VisioFiles = Get-ChildItem -Path $SourcePath -Recurse -Include *.vsdx,*.vssx,*.vstx,*.vxdm,*.vssm,*.vstm,*.vsd,*.vdw,*.vss,*.vst

    If($VisioFiles)
	{
		#Create the Visio application object
		$VisioApp = New-Object -ComObject Visio.Application
        $VisioApp.Visible = $false

        
		Foreach($File in $VisioFiles)
		{
            $Objs = @()
            $FilePath = $File.FullName
            #get the directory of file
			$FileDirectory = $File.DirectoryName 
			$FileBaseName = $File.BaseName

            Write-Verbose "Opening $FilePath file."
            $Document = $VisioApp.Documents.Open($FilePath)
            $Pages = $VisioApp.ActiveDocument.Pages
            $PagesCount = $Pages.count
            $PageName
            $intPageNumber = 1
            Try
            {
                Foreach($Page in $Pages)
                {
					$DestinationPath = "$FileDirectory\$($DestinationDirectory)\$($Page.Name).png"
                    Write-Progress -Activity "Converting Visio page file [$FileBaseName] to PNG file" `
           	        -Status "$intPageNumber of $PagesCount visio files - Finished" -PercentComplete $($intPageNumber/$PagesCount*100)

                    Write-Verbose "Converting Visio page file '$FilePath' to '$DestinationPath'"
                    $Page.Export($DestinationPath) 
                    $intPageNumber++

                    $Properties = @{'SourceFileName' = $File.Name
                                    'File Name' = $Page.Name
							        'Convert to PNG' = If(Test-Path -Path $DestinationPath)
													   {"Finished"}
													   Else
													   {"Unfinished"}
							        }		
			        $objVisio = New-Object -TypeName PSObject -Property $Properties
			        $Objs += $objVisio
                    
                }
                $Objs
                $Document.Close()
            }
            Catch
            {
		        Write-Warning "A few visio page files have been lost in this converting. NO.$intPageNumber visio page file cannot convert to png file."
            }
        }
        
        $VisioApp.Quit()
    }
    Else
    {
        Write-Warning "Please make sure that at least one Visio file in the ""$Path""."
    }
}
Else
{
    Write-Warning "The path does not exist, plese input the correct path."
}