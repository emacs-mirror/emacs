<?php
if(!defined("LCE_FUNCTIONS"))
{
  define("LCE_FUNCTIONS", 1);
  include("base.php");
  include("lce_config.php");

  // Unknown line class
  define("LCE_UNKNOWN", 0);
  // pure whitespace
  define("LCE_WS", 1);
  // a unqualified comment
  define("LCE_COMMENT", 2);
  // a user/translator comment
  define("LCE_COMMENT_USER", 3);
  // a tool-generated comment
  define("LCE_COMMENT_TOOL", 4);
  // A line containing a MSGID
  define("LCE_MSGID", 5);
  // A line containing a MSGSTR
  define("LCE_MSGSTR", 6);
  // A quoted text string
  define("LCE_TEXT", 7);

  define("STATE_ABORT", 0);
  define("STATE_OK", 1);
  define("STATE_LOOP", 2);

  class POEntryAD extends AD
    {
      function validate($value)
     {
       //	  print '"<pre>' . $value . '"<br></pre>';
       $result =  AD::validate(trim($value));
       //return $result;
       if($result[0])
	 {
	   $lines = explode("\n", ereg_replace("\r", "", $result[1]));
	   //$lines = explode("\n", $result[1]);
	   /*	      print "<pre>";
	   print_r($lines);
	   print "</pre>";*/
	   $res = array();
	   for($i = 0; $i < count($lines); $i++)
	     {
	       if(trim($lines[$i]) != "")
		 $res[] = $lines[$i];
	     }
	   $result[1] = join("\n", $res);
	   /*	      print "<pre>";
	   print_r($result[1]);
	   print "</pre>";*/

	   $result[0] = $this->checkQuotation($result[1]);
	 }
       return $result;
     }

      function checkQuotation($str)
     {
       $rex = "\\\\n|\\\\t|\\\\r|\\\\\"";
       $str = ereg_replace($rex, "", $str);
       $str = ereg_replace("\\\\\\\\", "", $str);
       return !(strstr($str, "\"")
		|| strstr($str, "\\"));
     }
    }


  class CommentAD extends AD
    {
      var $prefix;
      function CommentAD(
		      $name,			// the name of the variable
		      $not_null = 0,
		      $type = "",	// as returned by gettype
		      $prefix = "# ")
     {
       $this->prefix = $prefix;
       AD::AD($name, $not_null, $type);
     }

      function validate($value)
     {
       $res = AD::validate($value);
       return $res;
       if($res[0] && $res[1] != "")
	 {
	   $mod_lines = array();
	   $lines = explode("\n", $res[1]);

	   for($i = 0; $i < count($lines); $i++)
	     {
	       $line = $lines[$i];
	       if(substr($line, 0, 1) != "#")
		   $line = $this->prefix . $line;
	       $mod_lines[] = $line;
	     }
	   $res[1] = join("\n", $mod_lines);
	 }
       return $res;
     }
    }

  class POEntry extends HtmlValidator
    {
      var $msgid;
      var $msgstr;
      var $user_comment;
      var $sys_comment;
      var $unk_comment;

      var $msgid_lc = 0;
      var $msgstr_lc = 0;
      var $user_comment_lc = 0;
      var $sys_comment_lc = 0;
      var $unk_comment_lc = 0;

      function POEntry()
     {
       $this->atts = array(
			   new AD("msgid"),
			   new POEntryAD("msgstr", REQUIRED_ATTRIBUTE),
			   new CommentAD("user_comment"),
			   new POEntryAD("sys_comment"),
			   new POEntryAD("unk_comment"),
			   new AD("msgid_lc", NOT_REQUIRED_ATTRIBUTE, 0),
			   new AD("msgstr_lc", NOT_REQUIRED_ATTRIBUTE, 0),
			   new AD("user_comment_lc", NOT_REQUIRED_ATTRIBUTE, 0),
			   new AD("sys_comment_lc", NOT_REQUIRED_ATTRIBUTE, 0),
			   new AD("unk_comment_lc", NOT_REQUIRED_ATTRIBUTE, 0)
			   );
     }

      function lineCount($entry)
     {
       $lc = count(explode("\n", $entry));
       return $lc;
     }

      function serializeToVars($prefix)
     {
       $this->user_comment_lc = $this->lineCount($this->user_comment);
       $this->unk_comment_lc = $this->lineCount($this->sys_comment);
       $this->sys_comment_lc = $this->lineCount($this->unk_comment);
       $this->msgid_lc = $this->lineCount($this->msgid);
       $this->msgstr_lc = $this->lineCount($this->msgstr);
       return HtmlValidator::serializeToVars($prefix);
     }

      function write()
     {
       $content = "";
       $content .= $this->user_comment . "\n";
       $content .= $this->unk_comment . "\n";
       $content .= $this->sys_comment . "\n";
       $content .= "msgid \"" . $this->msgid . "\"\n";
       $content .= 'msgstr "' . join("\"\n\"", explode("\n", $this->msgstr)) . "\"" . "\n\n";
       return $content;
     }
    }

  class POReader extends HTMLValidator
    {
      var $msgid;
      var $msgstr;
      var $user_comment;
      var $sys_comment;
      var $unk_comment;
      var $state;
      var $ignore_ws;
      var $po_entries;
      var $poe_num;
      var $filename;
      var $domain;

      function gettext($msgid)
     {
       if(isset($this->po_entries[$msgid]))
	 {
	   $po = $this->po_entries[$msgid];
	   return StripCSlashes(join("", explode("\n", $po->msgstr)));
	   //return $po->msgstr;
	 }
       return $msgid;
     }


      function parseFromVars($prefix)
     {
       $res = HtmlValidator::parseFromVars($prefix);
       if($res[0])
	 {
	   $poe_res = true;
	   $this->po_entries = array();
	   for($i = 0; $i < $this->poe_num; $i++)
	     {
	       $poe = new POEntry;
	       $res = $poe->parseFromVars($prefix . "_POE$i");
	       if($res[0])
		 {
		   $msgid = $prefix . "_POE" . $i . "_MSGID";
		   $msgid = $$msgid;
		   $this->po_entries[$prefix . "_POE" . $i . "_MSGID"] = $res[1];
		 }
	       else
		 $poe_res = false;
	     }
	 }
       if(!$poe_res)
	 $GLOBALS[$prefix . "_ERR"] = 1;
       return array($poe_res, $this);
     }

      function serializeToVars($prefix)
     {
       HtmlValidator::serializeToVars($prefix);
       reset($this->po_entries);
       $i = 0;
       while($poe = each($this->po_entries))
	 {
	   $poe = $poe[1];
	   $poe->serializeToVars($prefix . "_POE$i");
	   $i++;
	 }
     }


      function POReader($domain, $filename)
     {
       $this->domain = $domain;
       $this->filename = $filename;
       $this->ignore_ws = true;
       $this->po_entries = array();
       $this->atts = array(
			   new AD("domain", REQUIRED_ATTRIBUTE),
			   new AD("filename", REQUIRED_ATTRIBUTE),
			   new AD("poe_num", REQUIRED_ATTRIBUTE, 0)
			   );
     }


      function read()
     {
       if($fh = fopen($this->filename, "r"))
	 {
	   $this->lines = array();
	   while (!feof ($fh))
	     {
	       $line = fgets($fh, 4096);
	       $this->lines[] = $line;
	     }
	   fclose($fh);
	 }
       $this->createPOEntries();
       $this->poe_num = count($this->po_entries);
     }

      function write($save="yes")
     {
       reset($this->po_entries);
       $content = "";
       while($poe = each($this->po_entries))
	 {
	   $poe = $poe[1];
	   $content .= $poe->write();
	 }

       if(($fh = fopen($this->filename, "w"))
	  && $save == "yes")
	 {
	   fwrite($fh, $content);
	   }
       return $content;
     }

      function isComment($class)
     {
       if($class == LCE_COMMENT || $class == LCE_COMMENT_USER || $class == LCE_COMMENT_TOOL)
	 return true;
       return false;
     }

      function comment($line, $class)
     {
       if($this->isComment($class))
	 {
	   if($class == LCE_COMMENT_USER)
	     $this->user_comment .= $line;
	   else if($class == LCE_COMMENT_TOOL)
	     $this->sys_comment .= $line;
	   else
	     $this->unk_comment .= $line;
	   return STATE_OK;
	 }
       if($class == LCE_MSGID)
	 {
	   $this->state = "msgid";
	   return STATE_LOOP;
	 }
       return STATE_ABORT;
     }

      function msgid($line, $class)
     {
       if($class == LCE_MSGID || $class == LCE_TEXT)
	 {
	   $line = $this->stripLine($line, LCE_MSGID);
	   $this->msgid .= $line;
	   return STATE_OK;
	 }
       if($class == LCE_MSGSTR)
	 {
	   $this->state = "msgstr";
	   return STATE_LOOP;
	 }
       return STATE_ABORT;
     }

      function msgstr($line, $class)
     {
       if($class == LCE_MSGSTR || $class == LCE_TEXT)
	 {
	   $line = $this->stripLine($line, $class);
	   $this->msgstr .= $line;
	   return STATE_OK;
	 }
       // We have a different state, so we have to create a POEntry
       $poe = new POEntry;
       $poe->user_comment = trim($this->user_comment);
       $poe->sys_comment = trim($this->sys_comment);
       $poe->unk_comment = trim($this->unk_comment);
       $poe->msgid = trim($this->msgid);
       $poe->msgstr = trim($this->msgstr);
       $this->po_entries[trim($this->msgid)] = $poe;
       $this->state = "start";
       return STATE_LOOP;
     }

      function start($line, $class)
     {
       $this->user_comment = "";
       $this->sys_comment = "";
       $this->unk_comment = "";
       $this->msgid = "";
       $this->msgstr = "";
       if($this->isComment($class))
	 {
	   $this->state = "comment";
	   return STATE_LOOP;
	 }
       if($class == LCE_MSGID)
	 {
	   $this->state = "msgid";
	   return STATE_LOOP;
	 }
       return STATE_OK;
     }

      function createPOEntries()
     {
       $this->msgid = "";
       $this->msgstr = "";
       $this->user_comment = "";
       $this->sys_comment = "";
       $this->state = "start";

       reset($this->lines);
       for($i = 0; $i < count($this->lines); $i++)
	 {
	   $line = $this->lines[$i];
	   $class = $this->classifyLine($line);
	   if($class != LCE_WS || !$this->ignore_ws)
	     {
	       $state_ret = STATE_LOOP;
	       while($state_ret == STATE_LOOP)
		 {
		   $state = $this->state;
		   //print "$this->state $class:$line <br>";
		   $state_ret = $this->$state($line, $class);
		 }
	       //print "state_ret = $state_ret <br>";
	     }
	   if($state_ret == STATE_ABORT)
	     break;
	 }
       // Get the last entry
       if($state_ret != STATE_ABORT)
	 {
	   $this->msgstr("", LCE_UNKNOWN);
	 }
     }

      function stripLine($line, $class)
     {
       switch($class)
	 {
	 case LCE_TEXT:
	   ereg('^"(.*)"', $line, $regs);
	   $line = $regs[1] . "\n";
	   break;
	 case LCE_MSGID:
	   if(substr($line, strlen("msgid")) == "msgid")
	     {
	       $line = substr($line, strlen("msgid") + 1);
	     }
	   ereg('"(.*)"', $line, $regs);
	   $line = $regs[1];
	   break;
	 case LCE_MSGSTR:
	   // TODO: Check if ^ can be removed
	   $line = substr($line, strlen("msgstr") + 1);
	   ereg('^"(.*)"', $line, $regs);
	   $line = $regs[1] . "\n";
	   break;

	 }
       return $line;
     }

      function printClassification()
     {
       reset($this->lines);
       for($i = 0; $i < count($this->lines); $i++)
	 {
	   $line = $this->lines[$i];
	   $class = $this->classifyLine($line);
	   print "#$i: $class $line<br>";
	 }
     }

      function classifyLine($line)
     {
       if(ereg("^[ \n\r\t]*$", $line))
	 return LCE_WS;
       if(ereg("^#.*\$", $line))
	 {
	   if(ereg("^[,:-~].*", substr($line, 1)))
	     {
	       return LCE_COMMENT_TOOL;
	     }
	   if(ereg("^[ \n\r\t].*", substr($line, 1)))
	     {
	       return LCE_COMMENT_USER;
	     }
	   return LCE_COMMENT;
	 }
       if(ereg("^msgid (.*)\$", $line, $regs))
	 {
	   $line = $regs[1];
	   if($this->classifyLine($line) == LCE_TEXT)
	     return LCE_MSGID;
	 }
       if(ereg("^msgstr (.*)\$", $line, $regs))
	 {
	   $line = $regs[1];
	   if($this->classifyLine($line) == LCE_TEXT)
	     return LCE_MSGSTR;
	 }
       if(ereg('^".*"', $line))
	 {
			     // TODO: Check correct escapes
	   return LCE_TEXT;
	 }

       return LCE_UNKNOWN;
     }
    }


  function getTextDomains($lines)
    {
      $default_domain = "";
      $domains = array();
      while($gl = each($GLOBALS))
     {
       $gname = $gl[0];
       global $$gname;
     }
      for($i = 0; $i < count($lines); $i++)
     {
       if(ereg("bindtextdomain\(([^,]+),([^\)]+)\)", $lines[$i], $regs))
	 {
			     //print "Line:" .  $lines[$i] . " <br>";
	   $name = $regs[1];
	   $ev = "\$directory = ". $regs[2] . ";";
	   print $ev;
	   eval($ev);
	   $domains[] = array($name, $directory);
	 }
       if(ereg("textdomain\(([^\)]+)\)", $lines[$i], $regs))
	 $default_domain = $regs[1];
     }
      return array($default_domain, $domains);
    }


  class PORManager extends HtmlValidator
    {
      var	$por_a;

      function PORManager()
     {
       $this->por_a = array();
     }

      function addPOReader($d_name, &$por)
     {
       $this->por_a[$d_name] = &$por;
     }

      function &getPOReader($domain)
     {
       return $this->por_a[$domain];
     }

      function getDomainNames()
     {
       return array_keys($this->por_a);
     }
    }

  function &loadPORManager()
    {
      global $LCE_PORMAN;
      if(!isset($LCE_PORMAN))
     {
       $LCE_PORMAN = new PORManager();
     }
      return $LCE_PORMAN;
    }


  // More or less intelligent filename joining
  // As available in PYTHONs os.path
  function fileJoin()
    {
      $numargs = func_num_args();
      $args = func_get_args();
      for($i = 0; $i < $numargs - 1; $i++)
     {
       if(substr($args[$i], -1) != "/")
	 $args[$i] = $args[$i] . "/";
       if($i > 0)
	 {
	   if(substr($args[$i],0 , 1) == "/")
	     $args[$i] = substr($args[$i], 1);
	 }

     }
      return join("", $args);
    }

  if(defined("LCE_TESTSERVER"))
    {

      function lce_bindtextdomain($d_name, $d_path)
     {
       global $LANG, $LC_MESSAGES, $LC_ALL, $LCE_LANG;
       global $LCE_ERR;
       global $LCE_PO_SUFFIX;
       global $LCE_MANAGER;

       $path_orig = $d_path;
       // This is not complete and reflects
       // my not very far going understanding of the
       // different $LC_x thingies.
       if(isset($LC_MESSAGES))
	 {
			     //print "LC_MESSAGES<br>";
	   $lang_suffix = $LC_MESSAGES;
	 }
       else if(isset($LC_ALL))
	 {
			     //print "LC_ALL<br>";
	   $lang_suffix = $LC_ALL;
	 }
       else if(isset($LANG))
	 {
			     //print "LANG<br>";
	   $lang_suffix = $LANG;
	 }
       else
	 {
			     //print "LCE_LANG<br>";
	   $lang_suffix = $LCE_LANG;
	 }

       //print "LangSuffix: $lang_suffix \n";
       //print "D_Path: " . fileJoin($d_path, $lang_suffix, "LC_MESSAGES", $d_name . $LCE_PO_SUFFIX) . "<br>";
       // First try: the whole lang_suffix

       if(file_exists(fileJoin($d_path, $lang_suffix, "LC_MESSAGES", $d_name . $LCE_PO_SUFFIX)))
	 $d_path = fileJoin($d_path, $lang_suffix, "LC_MESSAGES", $d_name . $LCE_PO_SUFFIX);
       else
	 {
	   $lang_suffix = substr($lang_suffix, 0, 2);
	   if(file_exists(fileJoin($d_path, $lang_suffix, "LC_MESSAGES", $d_name. $LCE_PO_SUFFIX)))
	     $d_path = fileJoin(fileJoin($d_path, $lang_suffix, "LC_MESSAGES", $d_name . $LCE_PO_SUFFIX));
	   else
	     {
	       $LCE_ERR = "No PO-file found";
	       return false;
	     }
	 }
       //print "D_Path: $d_path \n";
       $por = new POReader($d_name, $d_path, $path_orig);
       $por->read();
       $porman =& loadPORManager();
       $porman->addPOReader($d_name, $por);
       return true;
     }

      function lce_textdomain($domain)
     {
       global $LCE_DOMAIN;
       $LCE_DOMAIN = $domain;
     }

      function lce_gettext($msgid)
     {
       global $LCE_DOMAIN;
       return lce_dgettext($LCE_DOMAIN, $msgid);
     }

      function lce_dgettext($domain, $msgid)
     {
       $porman =& loadPORManager();
       if($por = &$porman->getPOReader($domain))
	 return $por->gettext($msgid);
       return $msgid;
     }

      function lce()
     {
       global $LCE_LCEDITLOC;
       $porman =& loadPORManager();
       $domains = $porman->getDomainNames();
       for($i = 0; $i < count($domains); $i++)
	 {
	   $por =& $porman->getPOReader($domains[$i]);
	   $domain = "domain=" . urlencode($por->domain);
	   $filename = "filename=" . urlencode($por->filename);
	   $url = $LCE_LCEDITLOC . "?" . $domain . "&" . $filename;
	   print "<a target=\"_blank\" href=\"" . $url . "\">Domain: $por->domain</a><br>";
	 }
     }
    }
  else
    {
      function lce_bindtextdomain($domain, $path)
     {
       bindtextdomain($domain, $path);
     }

      function lce_textdomain($domain)
     {
       textdomain($domain);
     }

      function lce_gettext($msgid)
     {
       return gettext($msgid);
     }

      function lce_dgettext($domain, $msgid)
     {
       return dgettext($domain, $msgid);
     }
      function lce()
     {
     }
    }


  function lce_geteditcode($type, $name, $text, $rows=2)
    {
      global $LCE_EDIT_LEVEL;
      $level_map = array("msgid" => 4,
		      "sys_comment" => 3,
		      "user_comment" => 2,
		      "msgstr" => 1
		      );
      if($level_map[$type] > $LCE_EDIT_LEVEL)
     {
       return "<input type=\"hidden\" name=\"" . $name . "\" value=\"" . $text . "\"><pre>\n" . $text . "\n</pre>";
     }
      else
     {
       return "<textarea name=\"" . $name . "\" rows=\"" . $rows . "\" cols=\"60\">" . $text . "</textarea>";
     }
    }
}
/*
  ;;; Local Variables: ***
  ;;; mode:C ***
  ;;; End: ***
*/
?>
