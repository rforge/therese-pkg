
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

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents.'<a href="#therese">(*)</a>'; } ?>

<!-- end of project description -->

<p><em>Reference:</em> Villa-Vialaneix N., San Cristobal M. (2013) <a href="https://www.nathalievilla.org/IMG/pdf/villavialaneix_etal_JdS2013.pdf">Consensus LASSO : inférence conjointe de réseaux de gènes dans des conditions expérimentales multiples</a>. In proceedings of: <a href="http://jds2013.sfds.asso.fr"><em>45e Journées de Statistique de la SFdS</em></a>, Toulouse, France, May, 27-31, p 40.</p>
<p>The current article is a short abstract, written in French, but a long type English version will soon be released.</p>

<p> The <strong>project summary page</strong> is <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a> and the package can be downloaded <a href="https://r-forge.r-project.org/R/?group_id=1714"><strong>here</strong></a>.</p>

<br>
<br>
<a name="therese"><p style="font-size:small">(*) "therese" stands for: "Trust the Holy Estimation of Regulatory nEtworks from Several Expression data"<p></a>
</body>
</html>
