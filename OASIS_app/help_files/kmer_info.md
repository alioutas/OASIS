---
title: "k-mer Info"
author: "Antonios Lioutas"
date: "12/8/2020"
output: html_document
---
**A k-mer is a substring of a DNA sequence of length k. We use [JELLYFISH](https://www.cbcb.umd.edu/software/jellyfish/) to count how many times each 18-mer substring in a given probe occurs in the genome it targets. The maximum value of all 18-mer counts is another way to control probe specifity, and can identify problematic substrings that other alignment approaches may miss. By default, [PaintSHOP](https://www.paintshop.io) uses a maximum k-mer count of 5. This can be changed using the slider, and the probe table and density plot will dynamically update**
