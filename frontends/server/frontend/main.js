import { Editor } from './editor.js';

const canvas = document.querySelector('#editor');

function main() {
  document.fonts.ready.then(() => {
    const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
    const editor = new Editor({
      canvas: canvas,
      fontName: 'Monospace',
      fontSize: 19,
      onLoaded: null,
      url: `${protocol}://${window.location.hostname}:${window.location.port}`,
      onExit: null,
      onClosed: null,
      onRestart: null,
      onUserInput: null,
    });

    editor.init();
  });
}

main();
