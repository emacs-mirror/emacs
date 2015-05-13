<?php

/*
  Classe creata da Santoro Diego.
  Per aiuti nella programmazione in PHP, PERL, C e ECMAScript contattatemi
  e-Mail vincenza.tralice@tiscali.it oppure santoro.diego@3000.it
  La classe ? ancora in fase beta.
*/

final class sendMail {

  const eMailAddressErrorMessage="L' e-Mail indicata non rispetta un formato valido.";
  const defaultSubject="this is the subject.";
  const defaultTextMessage="this is text message.";
  const defaultHtmlMessage="this is html message.";
  const defaultHeaderMessage="this is a multi-part message in MIME format.";

  private static $messageProperties=array(
    "charset" => array(
        "modifiable" => true,
        "values" => array(
            "iso-8859-1",
            "iso-8859-15",
            "utf-8",
            "utf-16"
        )
    ),
    "content-transfer-encoding" => array(
        "modifiable" => true,
        "values" => array(
            "7bit",
            "8bit",
            "quoted-printable"
        )
    )
  );

  private $attachmentProperties=array(
    "content-type" => array(
        "modifiable" => false,
         "values" => array(
            "application/octet-stream"
        )
    ),
    "content-transfer-encoding" => array(
        "modifiable" => false,
        "values" => array(
            "base64"
        )
    ),
    "content-disposition" => array(
        "modifiable" => true,
        "values" => array(
            "attachment",
            "inline"
        )
    )
  );

  private static $relatedProperties=array(
    "content-transfer-encoding" => array(
        "modifiable" => false,
        "values" => array(
            "base64"
        )
    )
  );

  private $html;
  private $text;

  private $related;
  private $attachments;

  public static function valid_eMailAddress($eMailAddress) {
   if(ereg("^[^@ ]+@[^@ ]+\.[^@ ]+$", $eMailAddress))
    return true;
   else
    return false;
  }

  public static function validContentId($contentId) {
   if(ereg("^[a-zA-Z0-9]+$", $contentId))
    return true;
   else
    return false;
  }

  public static function validContentKey($contentKey) {
   if(ereg("^[a-zA-Z0-9]+$", $contentKey))
    return true;
   else
    return false;
  }

  public static function mime_content_type($filename) {
   $mime=array(
    '.3dmf' => 'x-world/x-3dmf',
    '.a' => 'application/octet-stream',
    '.aab' => 'application/x-authorware-bin',
    '.xwd' => 'image/x-xwd',
    '.xyz' => 'chemical/x-pdb',
    '.z' => 'application/x-compressed',
    '.zip' => 'application/x-zip-compressed',
    '.zoo' => 'application/octet-stream',
    '.zsh' => 'text/x-script.zsh',
    '.css' => 'text/css'
   );
   return $mime[strrchr($filename, '.')];
  }

  private $from;
  private $to;
  private $subject;

  private $finalized;

  private $headerMessage;
  private $bodyMessage;

  private $boundaries;

  public function __construct($from, $to, $subject=self::defaultSubject) {

   // set from
   if(!self::valid_eMailAddress($from))
    die(self::eMailAddressErrorMessage);
   else
    $this->from=$from;

   // set to
   if(!self::valid_eMailAddress($to))
    die(self::eMailAddressErrorMessage);
   else
    $this->to=$to;

   // set subject
   $this->subject=$subject;

   // set text
   $this->text=array(
    "message" => self::defaultTextMessage,
    "properties" => array(
        "charset" => self::$messageProperties["charset"]["values"][0],
        "content-transfer-encoding" => self::$messageProperties["content-transfer-encoding"]["values"][0]
    )
   );

   // set html
   $this->html=array(
    "message" => self::defaultHtmlMessage,
    "properties" => array(
        "charset" => self::$messageProperties["charset"]["values"][0],
        "content-transfer-encoding" => self::$messageProperties["content-transfer-encoding"]["values"][1]
    )
   );

   // set related and attachments
   $this->related=array();
   $this->attachments=array();

   // set finalizater counter
   $this->finalized=false;

   $this->headerMessage="";
   $this->bodyMessage="";

   $this->boundaries=array(
    "multipart/alternative" => md5(uniqid(microtime())),
    "multipart/related" => md5(uniqid(microtime())),
    "multipart/mixed" => md5(uniqid(microtime()))
   );

  }

  public function setTo($to, &$errorString) {
   if(self::valid_eMailAddress($to)) {
    $this->to=$to;
    return true;
   } else {
    $errorString=eMailAddressErrorMessage;
    return false;
   }
  }

  public function setFrom($from, &$errorString) {
   if(self::valid_eMailAddress($from)) {
    $this->from=$from;
    return true;
   } else {
    $errorString=eMailAddressErrorMessage;
    return false;
   }
  }

  public function setSubject($subject=self::defaultSubject) {
   $this->subject=$subject;
  }

  public function setTextMessage($textMessage=self::defaultTextMessage) {
   $this->text["message"]=$textMessage;
  }

  public function setTextMessageProperty($key, $value, &$errorString) {

   $key=strtolower($key);
   $value=strtolower($value);

   if(isset(self::$messageProperties[$key])) {
    if(in_array($value, self::$messageProperties[$key]["values"])) {
     if(self::$messageProperties[$key]["modifiable"]) {
       $this->text["properties"][$key]=$value;
       return true;
     } else {
      $errorString="Il valore della propriet? indicata non ? modificabile.";
      return false;
     }
    } else {
     $errorString="Il valore indicato per questa propriet? non ? valido.";
     return false;
    }
   } else {
    $errorString="Non esiste questa propriet? per i messaggi html.";
    return false;
   }
  }

  public function setHtmlMessage($htmlMessage=self::defaultHtmlMessage) {
   $this->html["message"]=$htmlMessage;
  }

  public function setHtmlMessageProperty($key, $value, &$errorString) {

   $key=strtolower($key);
   $value=strtolower($value);

   if(isset(self::$messageProperties[$key])) {
    if(in_array($value, self::$messageProperties[$key]["values"])) {
     if(self::$messageProperties[$key]["modifiable"]) {
      $this->html["properties"][$key]=$value;
      return true;
     } else {
      $errorString="Il valore della propriet? indicata non ? modificabile.";
      return false;
     }
    } else {
     $errorString="Il valore indicato per questa propriet? non ? valido.";
     return false;
    }
   } else {
    $errorString="Non esiste questa propriet? per i messaggi html.";
    return false;
   }
  }

  public function addRelated($fileName, $relatedKey, $contentId, &$errorString) {
   if(is_file($fileName)) {
    if($fileHandle=fopen($fileName, "r")) {
     if(self::validContentId($contentId)) {
      if(!isset($this->related[$relatedKey])) {
       if(self::validContentKey($relatedKey)) {
        $this->related[$relatedKey]=array(
    "fileName" => basename($fileName),
    "properties" => array(
        "content-type" => self::mime_content_type($fileName),
        "content-transfer-encoding" => self::$relatedProperties["content-transfer-encoding"]["values"][0],
        "content-id" => $contentId
    ),
    "source" => base64_encode(
        fread($fileHandle, filesize($fileName))
    )
        );
        return true;
       } else {
        $errorString="L' id specificato non ? valido.";
        return false;
       }
      } else {
       $errorString="La chiave specificata ? gi? associata ad un altro related.";
       return false;
      }
     } else {
      $errorString="La chiave specificata per il related non ? valida.";
      return false;
     }
    } else {
     $errorString="Non ? possibile aprire il file indicato.";
     return false;
    }
   } else {
    $errorString="Il nome del file indicato non ? valido.";
    return false;
   }
  }

  public function setRelatedProperty($relatedKey, $key, $value, &$errorString) {

   $key=strtolower($key);
   $value=strtolower($value);
  
   if(isset(self::$relatedProperties[$key])) {
    if(in_array($value, self::$relatedProperties[$key]["values"])) {
     if(self::$relatedProperties[$key]["modifiable"]) {
      if(isset($this->related[$relatedKey])) {
       $this->related[$relatedKey]["properties"][$key]=$value;
       return true;
      } else {
       $errorString="Il related indicato non esiste.";
       return false;
      }
     } else {
      $errorString="Il valore della propriet? indicata non ? modificabile.";
      return false;
     }
    } else {
     $errorString="Il valore indicato per questa propriet? non ? valido.";
     return false;
    }
   } else {
    $errorString="Non esiste questa propriet? per i related.";
    return false;
   }
  }

  public function addAttachment($fileName, $attachmentKey, &$errorString) {
   if(is_file($fileName)) {
    if($fileHandle=fopen($fileName, "r")) {
     if(self::validContentKey($attachmentKey)) {
      if(!isset($this->attachments[$attachmentKey])) {
       $this->attachments[$attachmentKey]=array(
    "fileName" => basename($fileName),
    "properties" => array(
        "content-type" => self::$attachmentProperties["content-type"]["values"][0],
        "content-disposition" => self::$attachmentProperties["content-disposition"]["values"][0],
        "content-transfer-encoding" => self::$attachmentProperties["content-transfer-encoding"]["values"][0]
    ),
    "source" => base64_encode(
        fread($fileHandle, filesize($fileName))
    )
       );
       return true;
      } else {
       $errorString="La chiave specificata ? gi? associata ad un altro allegato.";
       return false;
      }
     } else {
      $errorString="La chiave specificata per l'allegato non ? valida.";
      return false;
     }
    } else {
     $errorString="Non ? possibile aprire il file indicato.";
     return false;
    }
   } else {
    $errorString="Il nome del file indicato non ? valido.";
    return false;
   }
  }

  public function setAttachmentProperty($attachmentKey, $key, $value, &$errorString) {

   $key=strtolower($key);
   $value=strtolower($value);

   if(isset(self::$attachmentProperties[$key])) {
    if(in_array($value, self::$attachmentProperties[$key]["values"])) {
     if(self::$attachmentProperties[$key]["modifiable"]) {
      if(isset($this->attachments[$attachmentKey])) {
       $this->attachments[$attachmentKey]["properties"][$key]=$value;
       return true;
      } else {
       $errorString="L'allegato indicato non esiste.";
       return false;
      }
     } else {
      $errorString="Il valore della propriet? indicata non ? modificabile.";
      return false;
     }
    } else {
     $errorString="Il valore indicato per questa propriet? non ? valido.";
     return false;
    }
   } else {
    $errorString="Non esiste questa propriet? per gli allegati.";
    return false;
   }
  }

  public function finalize(&$errorString) {
   if(!$this->finalized) {
    $this->headerMessage="from: ".($this->from)."\n";
    $this->headerMessage.="to: ".($this->to)."\n";
    $this->headerMessage.="subject: ".($this->subject)."\n";
    $this->headerMessage.="mime-version: 1.0\n";

    if(($countAttachments=count($this->attachments))>0) {
     $this->headerMessage.="content-type: multipart/mixed; boundary=\"".($this->boundaries["multipart/mixed"])."\"\n\n";
     $this->headerMessage.=self::defaultHeaderMessage;
     $this->headerMessage.="\n\n";

     $this->bodyMessage="--".($this->boundaries["multipart/mixed"])."\n";

     if(($countRelated=count($this->related))>0) {
      $this->bodyMessage.="content-type: multipart/related; type=\"multipart/alternative\"; boundary=\"".($this->boundaries["multipart/related"])."\"\n\n";

      $this->bodyMessage.="--".($this->boundaries["multipart/related"])."\n";

      $this->bodyMessage.="content-type: multipart/alternative; boundary=\"".($this->boundaries["multipart/alternative"])."\"\n\n";
      $this->createMultipartAlternativeMessage($this->boundaries["multipart/alternative"]);
      $this->bodyMessage.="--".($this->boundaries["multipart/alternative"])."--\n\n";

      // aggiungere i related e chiudere

      $relatedCounter=0;
      while(list($key,)=each($this->related)) {
       $relatedCounter++;
      
       $this->bodyMessage.="--".$this->boundaries["multipart/related"]."\n";
       $this->createMultipartRelatedMessage($key);
       if($relatedCounter!=$countRelated) $this->bodyMessage.="--".($this->boundaries["multipart/related"])."\n";
       else $this->bodyMessage.="--".($this->boundaries["multipart/related"])."--\n\n";
      }
     } else {
      $this->bodyMessage.="content-type: multipart/alternative; boundary=\"".($this->boundaries["multipart/alternative"])."\"\n\n";
      $this->createMultipartAlternativeMessage();
      $this->bodyMessage.="--".($this->boundaries["multipart/alternative"])."--\n\n";
     }

     $attachmentsCounter=0;
     while(list($key,)=each($this->attachments)) {
      $attachmentsCounter++;
      $this->bodyMessage.="--".($this->boundaries["multipart/mixed"])."\n";
      $this->createMultipartMixedMessage($key);
      if($attachmentsCounter!=$countAttachments) $this->bodyMessage.="--".($this->boundaries["multipart/mixed"])."\n";
      else $this->bodyMessage.="--".($this->boundaries["multipart/mixed"])."--\n\n";
     }
    } else {
     if(($countRelated=count($this->related))>0) {
      $this->headerMessage.="content-type: multipart/related; type=\"multipart/alternative\"; boundary=\"".($this->boundaries["multipart/related"])."\"\n\n";
      $this->headerMessage.=self::defaultHeaderMessage;
      $this->headerMessage.="\n\n";

      $this->bodyMessage="--".($this->boundaries["multipart/related"])."\n";
      $this->bodyMessage.="content-type: multipart/alternative; boundary=\"".($this->boundaries["multipart/alternative"])."\"\n\n";
      $this->createMultipartAlternativeMessage();
      $this->bodyMessage.="--".($this->boundaries["multipart/alternative"])."--\n\n";

      $relatedCounter=0;
      while(list($key,)=each($this->related)) {
       $relatedCounter++;
       $this->bodyMessage.="--".$this->boundaries["multipart/related"]."\n";
       $this->createMultipartRelatedMessage($key);
       if($relatedCounter!=$countRelated) $this->bodyMessage.="--".($this->boundaries["multipart/related"])."\n";
       else $this->bodyMessage.="--".($this->boundaries["multipart/related"])."--\n\n";
      }
     } else {
      $this->headerMessage.="content-type: multipart/alternative; boundary=\"".($this->boundaries["multipart/alternative"])."\"\n\n";
      $this->headerMessage.=self::defaultHeaderMessage;
      $this->headerMessage.="\n\n";

      $this->createMultipartAlternativeMessage();
      $this->bodyMessage.="--".($this->boundaries["multipart/alternative"])."--";

     }
    }
    $this->finalized=true;
    return true;
   } else {
    $errorString="Al momento non ? possibile finalizzare.";
    return false;
   }
  }

  private function createMultipartAlternativeMessage() {
   $multipartAlternativeBoundary=$this->boundaries["multipart/alternative"];
   $this->bodyMessage.="--$multipartAlternativeBoundary\n";
   $this->bodyMessage.="content-type: text/plain; charset=\"".($this->text["properties"]["charset"])."\"\n";
   $this->bodyMessage.="content-transfer-encoding: ".($this->text["properties"]["content-transfer-encoding"])."\n\n";
   $this->bodyMessage.=$this->text["message"];
   $this->bodyMessage.="\n\n";
   $this->bodyMessage.="--$multipartAlternativeBoundary\n";
   $this->bodyMessage.="content-type: text/html; charset=\"".($this->html["properties"]["charset"])."\"\n";
   $this->bodyMessage.="content-transfer-encoding: ".($this->html["properties"]["content-transfer-encoding"])."\n\n";
   $this->bodyMessage.=$this->html["message"];
   $this->bodyMessage.="\n\n";
  }

  private function createMultipartRelatedMessage($key) {
   $obj=$this->related[$key];
   $this->bodyMessage.="content-type: ".($obj["properties"]["content-type"])."; name=\"".($obj["fileName"])."\"\n";
   $this->bodyMessage.="content-transfer-encoding: ".($obj["properties"]["content-transfer-encoding"])."\n";
   $this->bodyMessage.="content-id: <".($obj["properties"]["content-id"]).">\n\n";
   $this->bodyMessage.=$obj["source"];
   $this->bodyMessage.="\n\n";
  }

  private function createMultipartMixedMessage($key) {
   $obj=$this->attachments[$key];
   $this->bodyMessage.="content-type: ".($obj["properties"]["content-type"])."; name=\"".($obj["fileName"])."\"\n";
   $this->bodyMessage.="content-transfer-encoding: ".($obj["properties"]["content-transfer-encoding"])."\n";
   $this->bodyMessage.="content-disposition: ".($obj["properties"]["content-disposition"])."; filename=\"".($obj["fileName"])."\"\n\n";
   $this->bodyMessage.=$obj["source"];
   $this->bodyMessage.="\n\n";
  }

  public function getSource(&$errorString) {
   if($this->finalized) {
    return ($this->headerMessage).($this->bodyMessage);
   } else {
    $errorString="Ancora non ? avvenuta la finalizzazione.";
    return false;
   }
  }

  public function sendMail(&$errorString) {
   if($this->finalized) {
    mail($this->to, $this->subject, $this->bodyMessage, $this->headerMessage);
    $this->finalized=false;
    return true;
   } else {
    $errorString="Ancora non ? avvenuta la finalizzazione.";
    return false;
   }
  }
}

?>
