![CLARA_LOGO_V2](https://github.com/user-attachments/assets/03f2881b-700e-430e-9701-a61838e97c06)
#
CLARA is a program designed to assist in the analysis and visualization of ELISA graphs for humoral response and BCA curves.  It features several functions, separated into sections to facilitate user understanding. The `Wiki` has explanations for each tab within the program.
#
### Installing and Using the application

1. Open Windows `PowerShell` as an administrator to install the `.R`, `RStudio`, and `Rtools` applications
```
winget install --id=RProject.R --version 4.5.1 -e --wait; winget install --id=Posit.RStudio --version 2025.05.1+513  -e --wait; winget install --id=RProject.Rtools --version 4.5.6608 -e --wait

```

>[!NOTE]
>If you already have the `.R` installed, download only `Rtools`
>```
> winget install --id=RProject.Rtools --version 4.5.6608 -e
>``` 

2. Creates a `Analise_R` directory, installs the `.zip`, opens that directory containing the application and installs necessary libraries
```
$DesktopPATH = [System.Environment]::GetFolderPath('Desktop'); $ProjectPath = Join-Path -Path $DesktopPATH -ChildPath "Analise_R"; mkdir $ProjectPath -ErrorAction SilentlyContinue; $ZipFilePath = Join-Path -Path $ProjectPath -ChildPath "CLARA.zip"; Invoke-WebRequest -Uri https://github.com/HGPegoraro/CLARA/archive/refs/heads/main.zip -OutFile $ZipFilePath; Expand-Archive -Path $ZipFilePath -DestinationPath $ProjectPath -Force; cd (Join-Path -Path $ProjectPath -ChildPath "CLARA-main"); cd .\main; Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope Process; .\SETUP_CLARA.ps1
```

3. Runs the program.
```
Rscript -e "shiny::runApp('05_app.R', launch.browser = TRUE)"
```
#
To open the program again after closing `Windows Powershell`
```
$DesktopPATH = [System.Environment]::GetFolderPath('Desktop'); $ProjectPath = Join-Path -Path $DesktopPATH -ChildPath "Analise_R"; cd (Join-Path -Path $ProjectPath -ChildPath "CLARA-main"); Rscript -e "shiny::runApp('05_app.R', launch.browser = TRUE)"
```
