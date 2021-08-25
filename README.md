# fcpsDash
1) Clone the repository
2) From the repo directory, run "docker build -t fcps .\"
3) After the fcps image builds, run "docker run -ti --rm -p 8080:8080 fcps Rscript FCPS_Dash.R"

The dashboard will now be available in your browser at http://127.0.0.1:8080

You can utilize the Measure Type radio buttons to change the visualization between new cases and new quarantines.
Using the Measure Period dropdown box, you can aggregate the data for the the past 0 (most recent day), 7, 14, 21, and 28 days. 
