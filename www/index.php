
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	  <style type="text/css">
    body, html {
      font-family: Helvetica, Arial, sans-serif;
      background-color: #F5F5F5;
      color: #114;
      margin: 0;
      padding: 0;
    }
    a {
      text-decoration: none;
    }
    a:hover {
      text-decoration: underline;
    }
    #titleBar {
      height: 80px;
      background-color: #F5A9F2;
      overflow: hidden;
      border-bottom: 1px solid #3475b3;
      -moz-box-shadow:    0px 0px 10px 3px #BBC;
      -webkit-box-shadow: 0px 0px 10px 3px #BBC;
      box-shadow:         0px 0px 10px 3px #BBC;
    }
    #titleBar #container {
      margin-top: 14px;
    }
    #titleBar h1 {
      margin: 0 auto .5em auto;
      padding: .2em;
      color: #EEE;
      text-align: center;
    }
    #intro {
      background-color: #DDD;
      margin: 1em 1em 0 1em;
      padding: .75em;
      text-align: center;
      border: 1px solid #CCC;
      font-size: 18px;
    }
    #intro p {
      margin: .3em 0 .3em 0;
    }
    #outer-content {
      max-width: 910px;
      margin-left: auto;
      margin-right: auto;
    }
    #content {
      margin: 1em auto 1em auto;
      float: left;
    }
    #main{
      margin-right: 350px;
      float: left;
      line-height: 24px;
    }

    #shiny{
      float: left;
      width: 305px;
      margin-left: -330px;
      padding-left: 20px;
      border-left: 1px solid #AAA;
    }
    #shiny iframe {
      margin-top: 30px;
    }
    .caption{
      font-size: 13px;
    }
    code {
      background-color: #E5E5E5;
      border: 1px solid #AAA;
      -webkit-border-radius: 3px;
      -khtml-border-radius: 3px; 
      -moz-border-radius: 3px;
      border-radius: 3px;
      padding: 0 .5em 0 .5em;
    }
  </style>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

  <div id="titleBar">
    <div id="container">
<h1>
<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
  $contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>
</h1>
</div>
</div>

<!-- end of project description -->

  <div id="outer-content">
    <div id="intro">
<ul>
<li>Find the <strong>project summary page</strong> <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. </li>
</ul>
</div>
<div id="outer-content">
    <div id="intro">
    <h2>Install therese</h2>
Installation is done by first installing a required package from CRAN:
<pre>
install.packages(c("igraph", repos="http://cran.r-project.org/")
</pre>
and then install therese directly from R-Forge:
<pre>
install.packages("therese", repos="http://R-Forge.R-project.org")
</pre>
or, alternatively, 
<pre>
install.packages("therese", repos="http://R-Forge.R-project.org", type="source")
</pre>
if you are using a Mac.<br>
In case installation from R-Forge does not work, download the package <a href="https://r-forge.r-project.org/R/?group_id=1714">here</a> and use installation from ZIP file in Windows GUI or for Unix users, the command line <code>R CMD INSTALL therese_XXX.tar.gz</code><br><br>
Do not hesitate to contact <a href="mailto:tuxette[AT]nathalievilla.org">tuxette</a> if you have a problem during the installation.
</div>
    <div id="content">
To cite the package, please use:
<ul>
<li>
Villa-Vialaneix N., Vignes M., Viguerie N., San Cristobal M. (2014) Inferring networks from multiple samples with consensus LASSO. <em>Quality Technology & Quantitative Management</em>, <b>11</b>(1), 39-60.</li>
</div>
</div>
</html>
