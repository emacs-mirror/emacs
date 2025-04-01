"use strict";

const modifierKeys = ["Shift", "Control", "Alt", "Meta"];

const convertKeyTable = {
  Enter: "Return",
  ArrowRight: "Right",
  ArrowLeft: "Left",
  ArrowUp: "Up",
  ArrowDown: "Down",

  '¡': '1',
  '™': '2',
  '£': '3',
  '¢': '4',
  '∞': '5',
  '§': '6',
  '¶': '7',
  '•': '8',
  'ª': '9',
  'º': '0',
  '–': '-',
  '≠': '=',
  '“': '[',
  '‘': ']',
  '«': '\\',
  '…': ';',
  'æ': '\'',
  '≤': ',',
  '≥': '.',
  '÷': '/',
  '⁄': '!',
  '€': '@',
  '‹': '#',
  '›': '$',
  'ﬁ': '%',
  'ﬂ': '^',
  '‡': '&',
  '°': '*',
  '·': '(',
  '‚': ')',
  '—': '_',
  '±': '+',
  '”': '{',
  '’': '}',
  '»': '|',
  'Ú': ':',
  'Æ': '"',
  '¯': '<',
  '˘': '>',
  '¿': '?',

  'œ': 'q',
  '∑': 'w',
  '´': 'e',
  '®': 'r',
  '†': 't',
  '¥': 'y',
  '¨': 'u',
  'ˆ': 'i',
  'ø': 'o',
  'π': 'p',
  'å': 'a',
  'ß': 's',
  '∂': 'd',
  'ƒ': 'f',
  '©': 'g',
  '˙': 'h',
  '∆': 'j',
  '˚': 'k',
  '¬': 'l',
  'Ω': 'z',
  '≈': 'x',
  'ç': 'c',
  '√': 'v',
  '∫': 'b',
  '˜': 'n',
  'µ': 'm',

  'Œ': 'Q',
  '„': 'W',
  '´': 'E',
  '‰': 'R',
  'ˇ': 'T',
  'Á': 'Y',
  '¨': 'U',
  'ˆ': 'I',
  'Ø': 'O',
  '∏': 'P',
  'Å': 'A',
  'Í': 'S',
  'Î': 'D',
  'Ï': 'F',
  '˝': 'G',
  'Ó': 'H',
  'Ô': 'J',
  '': 'K',
  'Ò': 'L',
  '¸': 'Z',
  '˛': 'X',
  'Ç': 'C',
  '◊': 'V',
  'ı': 'B',
  '˜': 'N',
  'Â': 'M',
};

function getKey(e) {
  if (e.altKey) {
    return (
      convertKeyTable[e.key] ||
      (e.code.startsWith('Key') ? e.code[3].toLowerCase() : null) ||
      e.key
    );
  }

  return convertKeyTable[e.key] || e.key;
}

export function convertKeyEvent (e) {
  if (modifierKeys.indexOf(e.key) !== -1) {
    return null;
  }

  const key = getKey(e);

  return {
    key: key,
    ctrl: e.ctrlKey,
    meta: e.altKey,
    super: e.metaKey,
    shift: e.shiftKey,
  };
};
