![CLARA_LOGO_V2](https://github.com/user-attachments/assets/03f2881b-700e-430e-9701-a61838e97c06)
#
CLARA is a program designed to assist in the analysis and visualization of ELISA graphs for humoral response and BCA curves.  It features several functions, separated into sections to facilitate user understanding. The `Wiki` has explanations for each tab within the program.
#
### Installing and Using the application

You only need to install one program (Docker Desktop). No R, RStudio or Rtools.

1. **Install Docker Desktop** from https://www.docker.com/products/docker-desktop/ and open it.
   Accept the terms and wait until it says Docker is running. On Windows you may be asked to restart the computer the first time.
2. **Download CLARA**: on this page click **Code → Download ZIP**. Right-click the file and choose **Extract All**, then move the folder somewhere simple, like Documents.
3. **Start CLARA**: open the folder and double-click `Start_CLARA.bat`.
   Mac/Linux: open Terminal in the folder and run `bash start_CLARA.sh`.

The first launch downloads CLARA and takes a few minutes. After that it starts in seconds.
Your browser opens CLARA automatically. If it doesn't, go to http://localhost:3838.
To stop CLARA, close the black window (or press Ctrl+C).

> **Windows warning:** if you see "Windows protected your PC", click **More info → Run anyway**.

### Where are my files?
- Excel plates: you choose them from your computer inside CLARA, as usual.
- Saved projects (.rds) and plots: downloaded by your browser, usually into the Downloads folder.
- Normality diagnosis images: in the `CLARA_output` folder next to `Start_CLARA.bat`.

### Updating
Just start CLARA again while online. It fetches the newest version automatically.

### Troubleshooting
- **"Docker Desktop is not running"**: open Docker Desktop, wait until it says running, then start CLARA again.
- **"port is already allocated"**: another CLARA window is still open. Close it, or restart Docker Desktop.
- **Docker Desktop won't start on Windows**: virtualization may be turned off in the BIOS. See Docker's Windows troubleshooting page.
