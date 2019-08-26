[This data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/)]

# About this story

**[How the Trump era is changing the federal bureaucracy](https://www.washingtonpost.com/politics/how-the-trump-era-is-changing-the-federal-bureaucracy/2017/12/30/8d5149c6-daa7-11e7-b859-fb0995360725_story.html)**

# About the folders in this repo

* **[data](data)** - Raw and summarized data on employment and separations figures from the [U.S. Office of Personnel Management](https://www.fedscope.opm.gov/)

# Notebooks

* [Permanent employees analysis](http://wpinvestigative.github.io/federal_employees_trump_2017/employment.html) - Comparing the employee bodycount for Trump and Obama in their first nine months in office (December - September)
* [Separations: Quit and Retired](http://wpinvestigative.github.io/federal_employees_2017/separations.html) - Comparing employees who have quit or retired under the first six months of Trump and Obama.

## Steve's Notes

This analysis does something interesting in `employment.Rmd` I've coded as *Resolve to arbitrary entity name among choices*. It has to do with how `agysubt` is defined around line 300.

```
# Combine them all but only take the first instance of the agency code
# This way there's a universal list of codes and agency names
# (Because agency names change year to year, like with abbreviations, depending on the whims of OPM)

agysubt <- rbind(dec_16_agysubt, sep_17_agysubt, dec_08_agysubt, sep_09_agysubt) %>%
    unique() %>%
    group_by(AGYSUB) %>%
    filter(row_number()==1)
```

Basically, you've got these agency codes, `AGYSUB` that don't change over time and these agency names, `AGYSUBT`, that do change. So you group by code and just stick with whatever name. Before the `filter` method, `agysubt` looks like this:

|   | AGYSUB | AGYSUBT |
| --| ------ | ------- |
| 4 | AF** | UNSPECIFIED |
| 5 | AF02 | AIR FORCE INSPECTION AGENCY (FO) |
| 6 | AF02 | AIR FORCE INSPECTION AND SAFETY CENTER |
| 7 | AF03 | AIR FORCE OPERATIONAL TEST AND EVALUATION CENTER |
| 8 | AF03 | AF OPERATIONAL TEST AND EVAL CTR |

So you could have reconsciled the names with the usual name entity resolution algorithms and manually picked out the keepers, but by using the relationship with another column and giving up a little control, you're able to do it in less than a second through this algorithmic-automated way.