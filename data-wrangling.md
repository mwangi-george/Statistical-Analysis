Wrangling LinkedIn Jobs Dataset
================
24 Nov, 2022

``` r
# remove scientific notation in output
options(
  scipen = 999
)


# load essential libraries
pacman::p_load(
  tidyverse,
  jsonlite,
  DataExplorer,
  SmartEDA,
  dlookr
)

# load dataset
jobs_data <- read_csv("datasets/linkedin_jobs_africa.csv",
                      show_col_types = F)

# create dataset metadata
metadata <- tibble(
  title = "Job title",
  company = "Name of the company",
  description = "description of the job and company",
  onsite_remote = "Location where the employee will be working from",
  salary = "Salary for the job. May be yearly or hourly. In most cases it is a range from min to max",
  location = "Where the company with the opening role is located",
  criteria = "Job requirements like experience, employment type, etc",
  posted_date = "The date the job was posted",
  link = "The URL to the job"
)

# preview metadata
metadata
```

<table>
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:left;">
company
</th>
<th style="text-align:left;">
description
</th>
<th style="text-align:left;">
onsite_remote
</th>
<th style="text-align:left;">
salary
</th>
<th style="text-align:left;">
location
</th>
<th style="text-align:left;">
criteria
</th>
<th style="text-align:left;">
posted_date
</th>
<th style="text-align:left;">
link
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Job title
</td>
<td style="text-align:left;">
Name of the company
</td>
<td style="text-align:left;">
description of the job and company
</td>
<td style="text-align:left;">
Location where the employee will be working from
</td>
<td style="text-align:left;">
Salary for the job. May be yearly or hourly. In most cases it is a range
from min to max
</td>
<td style="text-align:left;">
Where the company with the opening role is located
</td>
<td style="text-align:left;">
Job requirements like experience, employment type, etc
</td>
<td style="text-align:left;">
The date the job was posted
</td>
<td style="text-align:left;">
The URL to the job
</td>
</tr>
</tbody>
</table>

``` r
# pivot metadata into long format
metadata %>% 
  pivot_longer(
    # pivot all variables
    everything(),
    names_to = "variable",
    values_to = "description"
    # overwrite metadata
    ) -> metadata

# preview metadata
metadata
```

<table>
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:left;">
description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
title
</td>
<td style="text-align:left;">
Job title
</td>
</tr>
<tr>
<td style="text-align:left;">
company
</td>
<td style="text-align:left;">
Name of the company
</td>
</tr>
<tr>
<td style="text-align:left;">
description
</td>
<td style="text-align:left;">
description of the job and company
</td>
</tr>
<tr>
<td style="text-align:left;">
onsite_remote
</td>
<td style="text-align:left;">
Location where the employee will be working from
</td>
</tr>
<tr>
<td style="text-align:left;">
salary
</td>
<td style="text-align:left;">
Salary for the job. May be yearly or hourly. In most cases it is a range
from min to max
</td>
</tr>
<tr>
<td style="text-align:left;">
location
</td>
<td style="text-align:left;">
Where the company with the opening role is located
</td>
</tr>
<tr>
<td style="text-align:left;">
criteria
</td>
<td style="text-align:left;">
Job requirements like experience, employment type, etc
</td>
</tr>
<tr>
<td style="text-align:left;">
posted_date
</td>
<td style="text-align:left;">
The date the job was posted
</td>
</tr>
<tr>
<td style="text-align:left;">
link
</td>
<td style="text-align:left;">
The URL to the job
</td>
</tr>
</tbody>
</table>
