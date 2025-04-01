"use strict";

import { JSONRPC } from './jsonrpc.js';
import * as keyevent from './keyevent.js';
import * as meaw from 'meaw';

const textOffsetY = 5;

const isSafari = /^((?!chrome|android).)*safari/i.test(navigator.userAgent);

function isWideChar(c) {
  switch (meaw.getEAW(c)) {
    case 'A':
    case 'F':
    case 'W':
      return true;
    default:
      return false;
  }
}

function isMacOS() {
  return window.navigator.userAgent.indexOf('Mac OS X') !== -1;
}

function computeFontSize(font) {

  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  ctx.font = font;

  const textMetrics = ctx.measureText('W');

  return [
    Math.floor(textMetrics.width),
    Math.round(textMetrics.fontBoundingBoxAscent + textOffsetY + (textMetrics.emHeightDescent || 0)),
  ];
}

function drawBlock({ ctx, x, y, width, height, style }) {
  ctx.fillStyle = style;
  ctx.fillRect(x, y, width, height);
}

function drawText({ ctx, x, y, text, font, style, option }) {
  y += Math.round(textOffsetY); // 少しずらしておかないと上の部分が現在行からはみ出して、その行だけ再描画しても描画跡が残ってしまう
  ctx.fillStyle = style;
  ctx.font = font;
  ctx.textBaseline = 'top';

  for (const c of text) {
    if (isWideChar(c)) {
      ctx.fillText(c, x, y, option.fontWidth * 2);
      x += option.fontWidth * 2;
    } else {
      ctx.fillText(c, x, y, option.fontWidth);
      x += option.fontWidth;
    }
  }
}

function drawHorizontalLine({ ctx, x, y, width, style, lineWidth = 1 }) {
  ctx.strokeStyle = style;
  ctx.lineWidth = lineWidth;
  ctx.setLineDash = [];
  ctx.beginPath();
  ctx.moveTo(x, y);
  ctx.lineTo(x + width, y);
  ctx.stroke();
}

class Option {
  constructor({ fontName, fontSize }) {
    const font = fontSize + 'px ' + fontName;
    this.font = font;
    const [width, height] = computeFontSize(font);
    this.fontWidth = width;
    this.fontHeight = height;
    this.foreground = '#333';
    this.background = '#ccc';
  }
}

function getLemEditorElement() {
  return document.getElementById('lem-editor');
}

class Cursor {
  constructor(editor, name, color) {
    this.editor = editor;
    this.name = name;
    this.color = color;

    this.span = document.createElement('span');
    this.span.style.all = 'none';
    this.span.style.position = 'absolute';
    this.span.style.zIndex = '';
    this.span.style.top = '0';
    this.span.style.left = '0';
    this.span.style.fontFamily = editor.option.font;
    this.span.style.backgroundColor = color;
    this.span.style.color = 'white';
    this.span.innerHTML = '';
    document.body.appendChild(this.span);

    this.timerId = null;
  }

  move(x, y) {
    const [x0, y0] = this.editor.getDisplayRectangle();
    this.span.style.visibility = 'visible';
    this.span.textContent = this.name;
    this.span.style.left = (x + x0) + 'px';
    this.span.style.top = (y + y0) + 'px';
    this.span.style.padding = '3px 1%'
    if (this.timerId) {
      clearTimeout(this.timerId);
    }
    this.timerId = setTimeout(
      () => {
        this.span.style.visibility = 'hidden';
      },
      500,
    );
  }
}

function addMouseEventListeners({dom, editor, isDraggable, draggableStyle}) {
  dom.addEventListener('contextmenu', (event) => {
    event.preventDefault();
  });

  const handleMouseDownUp = (event, eventName) => {
    event.preventDefault();
    const [displayX, displayY] = editor.getDisplayRectangle();

    const pixelX = (event.clientX - displayX);
    const pixelY = (event.clientY - displayY);
    const x = Math.floor(pixelX / editor.option.fontWidth);
    const y = Math.floor(pixelY / editor.option.fontHeight);

    editor.jsonrpc.notify('input', {
      kind: eventName,
      value: {
        x: x,
        y: y,
        pixelX: pixelX,
        pixelY: pixelY,
        button: event.button,
        clicks: event.detail,
      }
    });
  };

  dom.addEventListener('mousedown', (event) => {
    if (isDraggable) document.body.style.cursor = draggableStyle;
    handleMouseDownUp(event, 'mousedown');
  });

  dom.addEventListener('mouseup', (event) => {
    if (isDraggable) document.body.style.cursor = 'default';
    handleMouseDownUp(event, 'mouseup');
  });

  let lastMouseMoveTime = 0;
  dom.addEventListener('mousemove', (event) => {
    event.preventDefault();
    const now = Date.now();
    if (now - lastMouseMoveTime > 50) {
      lastMouseMoveTime = now;
      const [displayX, displayY] = editor.getDisplayRectangle();

      const pixelX = (event.clientX - displayX);
      const pixelY = (event.clientY - displayY);
      const x = Math.floor(pixelX / editor.option.fontWidth);
      const y = Math.floor(pixelY / editor.option.fontHeight);
      editor.jsonrpc.notify('input', {
        kind: 'mousemove',
        value: {
          x: x,
          y: y,
          pixelX: pixelX,
          pixelY: pixelY,
          button: event.buttons === 0 ? null : event.buttons - 1,
        }
      });
    }
  });

  if (isDraggable) {
    dom.addEventListener('mouseover', () => {
      document.body.style.cursor = draggableStyle;
    });
    dom.addEventListener('mouseout', (e) => {
      if (e.buttons !== 1) {
        document.body.style.cursor = 'default';
      }
    });
  }

  dom.addEventListener('wheel', (event) => {
    event.preventDefault();
    const [displayX, displayY] = editor.getDisplayRectangle();
    const pixelX = (event.clientX - displayX);
    const pixelY = (event.clientY - displayY);
    const x = Math.floor(pixelX / editor.option.fontWidth);
    const y = Math.floor(pixelY / editor.option.fontHeight);

    editor.jsonrpc.notify('input', {
      kind: 'wheel',
      value: {
        pixelX: pixelX,
        pixelY: pixelY,
        x: x,
        y: y,
        wheelX: -Math.round(event.deltaX * 0.01),
        wheelY: -Math.round(event.deltaY * 0.01),
      },
    });
  });
}

const borderOffsetX = 5;
const borderOffsetY = 10;

class BaseSurface {
  constructor({ editor }) {
    this.editor = editor;
    this.mainDOM = null;
    this.wrapper = null;
  }

  delete() {
    if (this.wrapper) {
      getLemEditorElement().removeChild(this.wrapper);
    } else {
      getLemEditorElement().removeChild(this.mainDOM);
    }
  }

  setupDOM({ dom, isFloating, border }) {
    this.mainDOM = dom;

    if (isFloating && border) {
      this.wrapper = document.createElement('div');
      this.wrapper.style.position = 'absolute';
      this.wrapper.style.padding = '10px';
      this.wrapper.style.border = '1px solid';
      this.wrapper.style.borderColor = this.editor.option.foreground;
      this.wrapper.style.backgroundColor = this.editor.option.background;
      this.wrapper.appendChild(dom);
      getLemEditorElement().appendChild(this.wrapper);
    } else {
      getLemEditorElement().appendChild(dom);
    }
  }

  move(x, y) {
    const [x0, y0] = this.editor.getDisplayRectangle();
    const left = Math.floor(x0 + x * this.editor.option.fontWidth);
    const top = Math.floor(y0 + y * this.editor.option.fontHeight);
    if (this.wrapper) {
      this.wrapper.style.left = left - borderOffsetX + 'px';
      this.wrapper.style.top = top - borderOffsetY + 'px';
      this.mainDOM.style.left = borderOffsetX + 'px';
      this.mainDOM.style.top = borderOffsetY + 'px';
    } else {
      this.mainDOM.style.left = left + 'px';
      this.mainDOM.style.top = top + 'px';
    }
  }

  resize(width, height) {
    const ratio = window.devicePixelRatio || 1;
    this.mainDOM.width = width * this.editor.option.fontWidth * ratio;
    this.mainDOM.height = height * this.editor.option.fontHeight * ratio;
    this.mainDOM.style.width = width * this.editor.option.fontWidth + 'px';
    this.mainDOM.style.height = height * this.editor.option.fontHeight + 'px';

    const ctx = this.mainDOM.getContext('2d');
    ctx.scale(ratio, ratio);

    if (this.wrapper) {
      this.wrapper.style.width = width * this.editor.option.fontWidth + borderOffsetX * 2 + 'px';
      this.wrapper.style.height = height * this.editor.option.fontHeight + borderOffsetY * 2 + 'px';
    }
  }

  drawBlock(x, y, width, height, color) { }
  drawText(x, y, text, textWidth, attribute) { }

  touch() {
    return;
  }

  evalIn(code) {
    return eval(code);
  }
}

class CanvasSurface extends BaseSurface {
  constructor({ editor, view, x, y, width, height, styles, isFloating, border }) {
    super({ editor });

    const canvas = this.setupCanvas(styles);
    this.setupDOM({ dom: canvas, isFloating, border });
    this.move(x, y);
    this.resize(width, height);

    this.drawingQueue = [];

    addMouseEventListeners({dom:canvas, editor});
  }

  setupCanvas(styles) {
    const canvas = document.createElement('canvas');
    canvas.style.position = 'absolute';
    if (styles) {
      for (let key in styles) {
        canvas.style[key] = styles[key];
      }
    }
    return canvas;
  }

  drawBlock(x, y, width, height, color) {
    const option = this.editor.option;
    this.drawingQueue.push(function(ctx) {
      drawBlock({
        ctx,
        x: x * option.fontWidth,
        y: y * option.fontHeight,
        width: width * option.fontWidth,
        height: height * option.fontHeight,
        style: color,
      })
    });
  }

  drawText(x, y, text, textWidth, attribute) {
    const option = this.editor.option;
    this.drawingQueue.push(function(ctx) {
      if (!attribute) {
        drawBlock({
          ctx,
          x: x * option.fontWidth,
          y: y * option.fontHeight,
          width: textWidth * option.fontWidth,
          height: option.fontHeight,
          style: option.background,
        });
        drawText({
          ctx,
          x: x * option.fontWidth,
          y: y * option.fontHeight,
          text: text,
          style: option.foreground,
          font: option.font,
          option,
        });
      } else {
        let { foreground, background, bold, reverse, underline } = attribute;
        if (!foreground) {
          foreground = option.foreground;
        }
        if (!background) {
          background = option.background;
        }
        if (reverse) {
          const tmp = background;
          background = foreground;
          foreground = tmp;
        }
        const gx = x * option.fontWidth;
        const gy = y * option.fontHeight;
        drawBlock({
          ctx,
          x: gx,
          y: gy,
          width: textWidth * option.fontWidth,
          height: option.fontHeight,
          style: background,
        });
        drawText({
          ctx,
          x: gx,
          y: gy,
          text: text,
          style: foreground,
          font: bold ? ('bold ' + option.font) : option.font,
          option,
        });
        if (underline) {
          drawHorizontalLine({
            ctx,
            x: gx,
            y: gy + option.fontHeight - 2,
            width: textWidth * option.fontWidth,
            style: typeof (underline) === 'string' ? underline : foreground,
            lineWidth: 2
          });
        }
      }
    });
  }

  touch() {
    const ctx = this.mainDOM.getContext('2d');
    for (let fn of this.drawingQueue) {
      fn(ctx);
    }
    this.drawingQueue = [];
  }
}

class HTMLSurface extends BaseSurface {
  constructor({ editor, x, y, width, height, styles, isFloating, border, html }) {
    super({ editor });

    const iframe = document.createElement('iframe');
    this.setupDOM({ dom: iframe, isFloating, border });
    iframe.style.position = 'absolute';
    iframe.style.backgroundColor = 'white';
    iframe.setAttribute('sandbox', 'allow-scripts allow-same-origin');
    iframe.srcdoc = html;

    this.iframe = iframe;

    this.move(x, y);
    this.resize(width, height);
  }

  update(content) {
    const scrollY = this.iframe.contentWindow.scrollY;
    this.iframe.srcdoc = content;
    this.iframe.onload = () => {
      this.iframe.onload = null;
      this.iframe.contentWindow.scrollTo(0, scrollY);
    };
  }

  evalIn(code) {
    return this.iframe.contentWindow.eval(code);
  }
}

class VerticalBorder {
  constructor({ x, y, height, option, editor }) {
    this.option = option;
    this.editor = editor;
    this.line = document.createElement('div');
    this.line.style.backgroundColor = option.foreground;
    this.line.style.width = '5px';
    this.line.style.height = height * option.fontHeight + 'px';
    this.line.style.position = 'absolute';

    getLemEditorElement().appendChild(this.line);

    this.move(x, y);

    addMouseEventListeners({dom:this.line, editor, isDraggable: true, draggableStyle: 'col-resize'});
  }

  delete() {
    this.line.parentNode.removeChild(this.line);
  }

  move(x, y) {
    const [x0, y0] = this.editor.getDisplayRectangle();
    this.line.style.left = Math.floor(x0 + x * this.option.fontWidth - this.option.fontWidth / 2) + 'px';
    this.line.style.top = (y0 + y * this.option.fontHeight) + 'px';
  }

  resize(height) {
    this.line.style.height = height * this.option.fontHeight + 'px';
  }
}

const viewStyles = {
  tile: () => { },
  floating: (option) => ({
    boxSizing: 'border-box',
    borderColor: option.foreground,
    backgroundColor: option.background,
  }),
};

function getViewStyle(kind, option) {
  return viewStyles[kind](option) || {};
}

class View {
  constructor({
    id,
    x,
    y,
    width,
    height,
    useModeline,
    kind,
    type,
    content,
    border,
    borderShape,
    option,
    editor,
  }) {
    this.option = option;
    this.id = id;
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.useModeline = useModeline;
    this.kind = kind;
    this.type = type;
    this.border = border;
    this.borderShape = borderShape;
    this.editor = editor;

    this.leftsideBar = null;

    switch (kind) {
      case 'tile':
        this.mainSurface = this.makeSurface(type, content);
        this.leftSideBar = new VerticalBorder({
          x: x,
          y: y,
          height: height + (useModeline ? 1 : 0),
          option: option,
          editor: editor,
        });
        break;
      case 'floating':
        this.mainSurface = this.makeSurface(type, content);
        console.log(borderShape);
        if (borderShape === 'left-border') {
          this.leftSideBar = new VerticalBorder({
            x: x,
            y: y,
            height: height,
            option: option,
            editor: editor,
          });
        }
        break;
    }

    this.modelineSurface = useModeline ? this.makeModelineSurface() : null;
  }

  delete() {
    this.mainSurface.delete();
    if (this.modelineSurface) {
      this.modelineSurface.delete();
    }
    if (this.leftSideBar) {
      this.leftSideBar.delete();
    }
  }

  move(x, y) {
    this.x = x;
    this.y = y;

    this.mainSurface.move(x, y);
    if (this.modelineSurface) {
      this.modelineSurface.move(x, y + this.height);
    }
    if (this.leftSideBar) {
      this.leftSideBar.move(x, y);
    }
  }

  resize(width, height) {
    this.width = width;
    this.height = height;
    this.mainSurface.resize(width, height);
    if (this.modelineSurface) {
      this.modelineSurface.move(
        this.x,
        this.y + this.height,
      );
      this.modelineSurface.resize(width, 1);
    }
    if (this.leftSideBar) {
      this.leftSideBar.resize(height + 1);
    }
  }

  clear() {
    this.mainSurface.drawBlock(
      0,
      0,
      this.width,
      this.height,
      this.option.background,
    );
  }

  clearEol(x, y) {
    this.mainSurface.drawBlock(
      x,
      y,
      this.width - x,
      1,
      this.option.background,
    );
  }

  clearEob(x, y) {
    this.mainSurface.drawBlock(
      x, // x === 0
      y,
      this.width,
      this.height - y,
      this.option.background,
    );
  }

  print(x, y, text, textWidth, attribute) {
    this.mainSurface.drawText(
      x,
      y,
      text,
      textWidth,
      attribute,
    );
  }

  printToModeline(x, y, text, textWidth, attribute) {
    if (this.modelineSurface) {
      this.modelineSurface.drawText(
        x,
        y,
        text,
        textWidth,
        attribute,
      );
    }
  }

  touch() {
    this.mainSurface.touch();
    if (this.modelineSurface) {
      this.modelineSurface.touch();
    }
  }

  makeSurface(type, content) {
    switch (type) {
      case 'html':
        return this.makeHTMLSurface(content);
      case 'editor':
        return this.makeEditorSurface();
      default:
        console.error(`unknown type: ${type}`);
    }
  }

  makeHTMLSurface(content) {
    return new HTMLSurface({
      editor: this.editor,
      x: this.x,
      y: this.y,
      width: this.width,
      height: this.height,
      styles: getViewStyle(this.kind, this.option),
      isFloating: this.kind === 'floating',
      border: this.border,
      html: content,
    });
  }

  makeEditorSurface() {
    return new CanvasSurface({
      option: this.editor.option,
      x: this.x,
      y: this.y,
      width: this.width,
      height: this.height,
      styles: getViewStyle(this.kind, this.option),
      editor: this.editor,
      border: this.borderShape === 'left-border' ? 0 : this.border,
      isFloating: this.kind === 'floating',
      view: this,
    })
  }

  makeModelineSurface() {
    const surface = new CanvasSurface({
      option: this.editor.option,
      x: this.x,
      y: this.y + this.height,
      width: this.width,
      height: 1,
      editor: this.editor,
      view: this,
    });
    addMouseEventListeners({
      dom: surface.mainDOM,
      editor: this.editor,
      isDraggable: true,
      draggableStyle: 'row-resize',
    });
    return surface;
  }

  changeToHTMLContent(content) {
    if (this.mainSurface.constructor.name === 'HTMLSurface') {
      this.mainSurface.update(content);
    } else {
      this.mainSurface.delete();
      this.mainSurface = this.makeHTMLSurface(content);
    }
  }

  changeToEditorContent() {
    this.mainSurface.delete();
    this.mainSurface = this.makeEditorSurface();
  }

  evalIn(code) {
    return this.mainSurface.evalIn(code);
  }
}

function isPasteKeyEvent(event) {
  if (isMacOS()) {
    // TODO
    return false;
  } else {
    return (event.ctrlKey && event.shiftKey && event.key === 'V');
  }
}

class Input {
  constructor(editor) {
    const option = editor.option;
    this.editor = editor;

    this.composition = false;
    this.ignoreKeydownAfterCompositionend = false;

    this.span = document.createElement('span');
    this.span.style.color = option.foreground;
    this.span.style.backgroundColor = option.background;
    this.span.style.position = 'absolute';
    this.span.style.zIndex = '';
    this.span.style.top = '0';
    this.span.style.left = '0';
    this.span.style.font = option.font;

    this.input = document.createElement('input');
    this.input.style.backgroundColor = 'transparent';
    this.input.style.color = 'transparent';
    this.input.style.width = '0';
    this.input.style.padding = '0';
    this.input.style.margin = '0';
    this.input.style.border = 'none';
    this.input.style.position = 'absolute';
    this.input.style.zIndex = '-10';
    this.input.style.top = '0';
    this.input.style.left = '0';
    this.input.style.font = option.font;

    this.input.addEventListener('blur', (event) => {
      if (this.editor.inputEnabled) {
        this.input.focus();
      }
    });

    this.input.addEventListener('input', (event) => {
      //console.log('input', event);
      if (this.editor.inputEnabled) {
        if (this.composition === false) {
          this.input.value = '';
          this.span.innerHTML = '';
          this.input.style.width = '0';
          if (!isMacOS()) {
            this.editor.emitInputString(event.data);
          }
          //console.log('>>> input', event.data);
        }
      }
    });

    this.input.addEventListener('keydown', (event) => {
      //console.log('keydown', event, event.isComposing, this.composition);
      if (this.editor.inputEnabled) {

        if (isPasteKeyEvent(event)) {
          this.editor.jsonrpc.notify('input', { kind: 'clipboard-paste' });
          return;
        }

        if (event.isComposing || this.composition) {
          return;
        }

        // IMEに変換を指示する値はlemに渡すと誤作動するのでreturnする
        if (event.key === 'Process') {
          return;
        }

        if (this.ignoreKeydownAfterCompositionend && isSafari) {
          // safariではIMの入力中にバックスペースやエンターキーの入力によってcompositionendイベントが来ると
          // その後にkeydownイベントが即座に来る
          // それを処理してしまうと余分に改行されたり文字が消えるので無視する
          event.preventDefault();
          this.ignoreKeydownAfterCompositionend = false;
          return;
        }

        if (!isMacOS()) {
          // 修飾キーなしで、ReturnやBackspaceではなく'a'などの入力であるか(event.key.length === 1)
          if (!event.ctrlKey && !event.altKey && event.key.length === 1) {
            // そうであれば'input' eventが受け取れるようにここでreturnする
            return;
          }
        }

        event.preventDefault();

        if (event.isComposing !== true && event.code !== '') {
          // mac/chromium系のブラウザで"あ"と入力すると、
          // keydownの後にcompositionstartが来るので、
          // それを逆転するためにsetTimeoutを使う
          setTimeout(() => {
            if (!this.composition) {
              this.editor.emitInput(event);
              this.input.value = '';
            }
          }, 0);
          return false;
        }
      }
    });

    this.input.addEventListener('compositionstart', (event) => {
      //console.log('compositionstart', event);
      if (this.editor.inputEnabled) {
        this.composition = true;
        this.span.innerHTML = this.input.value;
        this.input.style.width = this.span.offsetWidth + 'px';
      }
    });

    this.input.addEventListener('compositionupdate', (event) => {
      //console.log('compositionupdate', event);
      if (this.editor.inputEnabled) {
        this.span.innerHTML = event.data;
        this.input.style.width = this.span.offsetWidth + 'px';
      }
    });

    this.input.addEventListener('compositionend', (event) => {
      //console.log('compositionend', event);
      if (this.editor.inputEnabled) {
        this.composition = false;
        this.editor.emitInputString(this.input.value);
        this.input.value = '';
        this.span.innerHTML = this.input.value;
        this.input.style.width = '0';
        this.ignoreKeydownAfterCompositionend = true;
      }
    });

    document.body.appendChild(this.input);
    document.body.appendChild(this.span);
    this.input.focus();
  }

  finalize() {
    document.body.removeChild(this.input);
    document.body.removeChild(this.span);
  }

  move(left, top) {
    const [x0, y0] = this.editor.getDisplayRectangle();
    this.span.style.top = (y0 + top) + 'px';
    this.span.style.left = (x0 + left) + 'px';
    this.input.style.top = this.span.offsetTop + 'px';
    this.input.style.left = this.span.offsetLeft + 'px';
  }

  updateForeground(color) {
    this.span.style.color = color;
  }

  updateBackground(color) {
    this.span.style.backgroundColor = color;
  }
}

class MessageTable {
  constructor() {
    this.map = new Map();
  }

  register(jsonrpc, table) {
    for (const method in table) {
      const handler = table[method];
      this.map.set(method, handler);
      jsonrpc.on(method, handler);
    }
  }

  get(method) {
    return this.map.get(method);
  }
}

function getDisplayRectangleDefault() {
  return [0, 0, window.innerWidth, window.innerHeight];
}

export class Editor {
  constructor({
    getDisplayRectangle = getDisplayRectangleDefault,
    fontName,
    fontSize,
    onLoaded,
    url,
    onExit,
    onClosed,
    onRestart,
    onUserInput,
    onSwitchFile,
  }) {
    this.getDisplayRectangle = getDisplayRectangle;

    this.option = new Option({ fontName, fontSize });

    this.onExit = onExit;
    this.onLoaded = onLoaded;
    this.onRestart = onRestart;
    this.onUserInput = onUserInput;
    this.onSwitchFile = onSwitchFile;
    this.inputEnabled = true;

    this.input = new Input(this);
    this.cursors = new Map();

    this.viewMap = new Map();

    this.jsonrpc = new JSONRPC(url, {
      onClosed: () => {
        onClosed();
      },
    });

    this.messageTable = new MessageTable();
    this.messageTable.register(this.jsonrpc, {
      'startup': this.startup.bind(this),
      'update-foreground': this.updateForeground.bind(this),
      'update-background': this.updateBackground.bind(this),
      'make-view': this.makeView.bind(this),
      'delete-view': this.deleteView.bind(this),
      'resize-view': this.resize.bind(this),
      'move-view': this.move.bind(this),
      'redraw-view-after': this.redrawViewAfter.bind(this),
      'clear': this.clear.bind(this),
      'clear-eol': this.clearEol.bind(this),
      'clear-eob': this.clearEob.bind(this),
      'put': this.put.bind(this),
      'modeline-put': this.modelinePut.bind(this),
      'update-display': this.updateDisplay.bind(this),
      'move-cursor': this.moveCursor.bind(this),
      'change-view': this.changeView.bind(this),
      'resize-display': this.resizeDisplay.bind(this),
      'bulk': this.bulk.bind(this),
      'exit': this.exitEditor.bind(this),
      'user-input': this.userInput.bind(this),
      'switch-file': this.switchFile.bind(this),
      'get-clipboard-text': this.getClipboardText.bind(this),
      'set-clipboard-text': this.setClipboardText.bind(this),
      'js-eval': this.jsEval.bind(this),
    });

    this.login();

    this.boundedHandleResize = this.handleResize.bind(this);
  }

  init() {
    window.addEventListener('resize', this.boundedHandleResize);
    document.getElementsByTagName('html')[0].style['background-color'] = '#333';
  }

  finalize() {
    window.removeEventListener('resize', this.boundedHandleResize);
    this.input.finalize();
  }

  closeConnection() {
    this.jsonrpc.close();
  }

  emitInput(event) {
    const key = keyevent.convertKeyEvent(event);
    if (!key) return;

    if (key.key === ']' && key.ctrl && !key.meta && !key.super && !key.shift) {
      this.jsonrpc.notify('input', { kind: 'abort' });
      return;
    }

    this.jsonrpc.notify('input', { kind: 'key', value: key });
  }

  emitInputString(string) {
    if (string) {
      this.jsonrpc.notify('input', { kind: 'input-string', value: string });
    } else {
      console.error('unexpected argument', string);
    }
  }

  handleResize(event) {
    const canResize = true;
    if (canResize) {
      this.jsonrpc.notify('redraw', { size: this.getDisplaySize() });
    } else {
      this.jsonrpc.notify('redraw');
    }
  }

  enableInput() {
    this.inputEnabled = true;
  }

  disableInput() {
    this.inputEnabled = false;
  }

  sendNotification(method, args) {
    this.jsonrpc.notify(method, args);
  }

  request(method, args, callback) {
    this.jsonrpc.request(method, args, callback);
  }

  getDisplaySize() {
    const [_x, _y, displayWidth, displayHeight] = this.getDisplayRectangle();
    const width = Math.round(displayWidth / this.option.fontWidth);
    const height = Math.round(displayHeight / this.option.fontHeight);
    return { width, height };
  }

  callMessage(method, argument) {
    this.messageTable.get(method)(argument);
  }

  findViewById(id) {
    return this.viewMap.get(id);
  }

  login() {
    this.jsonrpc.request('login', {
      size: this.getDisplaySize(),
      foreground: this.option.foreground,
      background: this.option.background,
    }, (response) => {
      this.updateForeground(response.foreground);
      this.updateBackground(response.background);
      if (response.views) {
        for (const view of response.views) {
          this.makeView(view);
        }
      }

      this.jsonrpc.notify('redraw', { size: this.getDisplaySize() });

      this.jsonrpc.request('user-file-map', {}, response => {
        this.onSwitchFile(response);
      });
    });
  }

  startup() {
    if (this.onRestart) {
      this.onRestart();
    }
  }

  updateForeground(color) {
    this.option.foreground = color;
    this.input.updateForeground(color);
  }

  updateBackground(color) {
    this.option.background = color;
    this.input.updateBackground(color);
    const element = getLemEditorElement();
    element.style.backgroundColor = color;
  }

  makeView({ id, x, y, width, height, use_modeline, kind, type, content, border, border_shape }) {
    const view = new View({
      option: this.option,
      id: id,
      x: x,
      y: y,
      width: width,
      height: height,
      useModeline: use_modeline,
      kind: kind,
      type: type,
      content: content,
      border: border,
      borderShape: border_shape,
      editor: this
    });
    this.viewMap.set(id, view);
  }

  deleteView({ viewInfo: { id } }) {
    const view = this.findViewById(id);
    view.delete();
    this.viewMap.delete(id);
  }

  resize({ viewInfo: { id }, width, height }) {
    const view = this.findViewById(id);
    view.resize(width, height);
  }

  move({ viewInfo: { id }, x, y }) {
    const view = this.findViewById(id);
    view.move(x, y);
  }

  redrawViewAfter({ viewInfo: { id }, html }) {
    const view = this.findViewById(id);
    view.touch();
  }

  clear({ viewInfo: { id } }) {
    const view = this.findViewById(id);
    view.clear();
  }

  clearEol({ viewInfo: { id }, x, y }) {
    const view = this.findViewById(id);
    view.clearEol(x, y);
  }

  clearEob({ viewInfo: { id }, x, y }) {
    const view = this.findViewById(id);
    view.clearEob(x, y);
  }

  put({ viewInfo: { id }, x, y, text, textWidth, attribute, cursorInfo }) {
    const view = this.findViewById(id);
    view.print(x, y, text, textWidth, attribute);
    if (cursorInfo) {
      const { name, color } = cursorInfo;
      let cursor = this.cursors.get(name);
      if (!cursor) {
        cursor = new Cursor(this, name, color);
        this.cursors.set(name, cursor);
      }

      cursor.move(
        (view.x + x) * this.option.fontWidth,
        (view.y + y - 1) * this.option.fontHeight,
      );
    }
  }

  modelinePut({ viewInfo: { id }, x, y, text, textWidth, attribute }) {
    const view = this.findViewById(id);
    view.printToModeline(x, y, text, textWidth, attribute);
  }

  updateDisplay() {
  }

  moveCursor({ viewInfo: { id }, x, y }) {
    const view = this.findViewById(id);
    const left = view.x * this.option.fontWidth + x * this.option.fontWidth;
    const top = view.y * this.option.fontHeight + y * this.option.fontHeight;
    this.input.move(left, top);
  }

  changeView({ viewInfo: { id }, type, content }) {
    const view = this.findViewById(id);
    switch (type) {
      case 'html':
        view.changeToHTMLContent(content);
        break;
      case 'editor':
        view.changeToEditorContent();
        break;
    }
  }

  resizeDisplay({ width, height }) {
    // TODO: offset
    const element = getLemEditorElement();
    element.style.width = Math.floor(width * this.option.fontWidth) + 'px';
    element.style.height = Math.floor(height * this.option.fontHeight) + 'px';
  }

  bulk(messages) {
    if (this.onLoaded) {
      this.onLoaded();
      this.onLoaded = null;
    }
    for (const { method, argument } of messages) {
      this.callMessage(method, argument);
    }
  }

  exitEditor() {
    if (this.onExit) {
      this.onExit();
    }
  }

  userInput({ value }) {
    if (this.onUserInput) {
      this.onUserInput(value);
    }
  }

  switchFile(userFileMap) {
    if (this.onSwitchFile) {
      this.onSwitchFile(userFileMap);
    }
  }

  getClipboardText() {
    navigator.clipboard.readText().then(text => {
      this.jsonrpc.notify('got-clipboard-text', { text });
    });
  }

  setClipboardText({ text }) {
    if (navigator.clipboard) {
      navigator.clipboard.writeText(text);
    }
  }

  jsEval({ viewInfo: { id }, code }) {
    const view = this.findViewById(id);
    const result = view.evalIn(code);
    if (result) {
      return result.toString();
    }
    return result;
  }
}
