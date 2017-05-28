#--------------------------------------------------------------------------------- 
#The sample scripts are not supported under any Microsoft standard support 
#program or service. The sample scripts are provided AS IS without warranty  
#of any kind. Microsoft further disclaims all implied warranties including,  
#without limitation, any implied warranties of merchantability or of fitness for 
#a particular purpose. The entire risk arising out of the use or performance of  
#the sample scripts and documentation remains with you. In no event shall 
#Microsoft, its authors, or anyone else involved in the creation, production, or 
#delivery of the scripts be liable for any damages whatsoever (including, 
#without limitation, damages for loss of business profits, business interruption, 
#loss of business information, or other pecuniary loss) arising out of the use 
#of or inability to use the sample scripts or documentation, even if Microsoft 
#has been advised of the possibility of such damages 
#--------------------------------------------------------------------------------- 

#requires -Version 2.0

<#
 	.SYNOPSIS
        This script can be used to batch covert Visio file to png file automatically.
    .DESCRIPTION
        This script can be used to batch covert Visio file to png file automatically.
    .PARAMETER  SourcePath
		Specifies the path of visio file.
    .EXAMPLE
        PS C:\Users\Administrator> C:\Script\ConvertVisioToPNG.ps1 -SourcePath C:\VisioFiles

        Convert to PNG                          File Name                               SourceFileName
        --------------                          ---------                               --------------
        Finished                                Page-1                                  MyVisio - 1.vsdx
        Finished                                Page-2                                  MyVisio - 1.vsdx
        Finished                                Page-3                                  MyVisio - 1.vsdx
        Finished                                Page-4                                  MyVisio - 1.vsdx
        Finished                                Page-5                                  MyVisio - 1.vsdx

    .EXAMPLE
        PS C:\Users\Administrator> C:\Script\ConvertVisioToPNG.ps1 -SourcePath 'C:\VisioFiles\MyVisio - 1.vsdx'

        Convert to PNG                          File Name                               SourceFileName
        --------------                          ---------                               --------------
        Finished                                Page-1                                  MyVisio - 1.vsdx
        Finished                                Page-2                                  MyVisio - 1.vsdx
        Finished                                Page-3                                  MyVisio - 1.vsdx
        Finished                                Page-4                                  MyVisio - 1.vsdx
        Finished                                Page-5                                  MyVisio - 1.vsdx
#>
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