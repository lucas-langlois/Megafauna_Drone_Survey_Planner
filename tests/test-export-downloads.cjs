const { test } = require('node:test');
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const path = require('node:path');
const source = fs.readFileSync(path.join(__dirname, '../www/export-downloads.js'), 'utf8');

function harness({ supported = true, permission = 'granted', header = 'attachment; filename="Bowen_M3E_Complete_Plan.zip"' } = {}) {
  const events = {}, nodes = {}, files = new Map();
  let fetchCount = 0, stopped = false;
  const folder = {
    name: 'Survey exports',
    requestPermission: async () => permission,
    getFileHandle: async (name, options) => {
      if (!files.has(name) && !options?.create) {
        const error = new Error('Missing'); error.name = 'NotFoundError'; throw error;
      }
      return { createWritable: async () => ({
        write: async blob => files.set(name, blob), close: async () => {}, abort: async () => {}
      }) };
    }
  };
  const document = {
    getElementById: id => nodes[id] ||= { textContent: '', addEventListener: (type, fn) => events[id + type] = fn },
    addEventListener: (type, fn) => events[type] = fn
  };
  const window = {
    isSecureContext: true,
    location: { href: 'https://example.shinyapps.io/planner/', origin: 'https://example.shinyapps.io' }
  };
  if (supported) window.showDirectoryPicker = async () => folder;
  vm.runInNewContext(source, { document, window, URL, fetch: async () => {
    fetchCount++;
    return { ok: true, headers: { get: () => header }, blob: async () => ({ size: 42 }) };
  } });
  events.DOMContentLoaded();
  return {
    events, nodes, files, window,
    get fetchCount() { return fetchCount; },
    get stopped() { return stopped; },
    choose: () => events.choose_export_folderclick(),
    download: () => events.click({ target: { closest: () => ({
      id: 'download_complete_plan', href: window.location.href + 'session/abc/download/complete_plan',
      classList: { contains: () => false }, getAttribute: () => '/planner/session/abc/download/complete_plan'
    }) }, preventDefault: () => stopped = true, stopImmediatePropagation: () => {} })
  };
}

test('normal downloads work without folder selection and in unsupported browsers', async () => {
  for (const supported of [true, false]) {
    const app = harness({ supported });
    await app.download();
    assert.equal(app.stopped, false);
    assert.equal(app.fetchCount, 0);
    if (!supported) assert.equal(app.nodes.choose_export_folder.hidden, true);
  }
});
test('chosen folder receives exports without overwriting existing files', async () => {
  const app = harness();
  await app.choose();
  await app.download();
  await app.download();
  assert.equal(app.stopped, true);
  assert.deepEqual([...app.files.keys()], ['Bowen_M3E_Complete_Plan.zip', 'Bowen_M3E_Complete_Plan_2.zip']);
  assert.match(app.nodes.export_folder_status.textContent, /Saved.*Survey exports/);
  app.events.clear_export_folderclick();
  await app.download();
  assert.equal(app.fetchCount, 2);
});
test('denied permission and invalid server responses do not write files', async () => {
  for (const options of [{ permission: 'denied' }, { header: null }, { header: 'attachment; filename="../bad.zip"' }]) {
    const app = harness(options);
    await app.choose();
    await app.download();
    assert.equal(app.files.size, 0);
    assert.match(app.nodes.export_folder_status.textContent, /Export not saved/);
  }
});
test('cancelling folder selection leaves normal downloads available', async () => {
  const app = harness();
  app.window.showDirectoryPicker = async () => { const error = new Error(); error.name = 'AbortError'; throw error; };
  await app.choose();
  await app.download();
  assert.equal(app.stopped, false);
});
