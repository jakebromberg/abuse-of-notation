import { defineConfig } from 'vite';
import { createHighlighter } from 'shiki';

// Vite plugin: apply Shiki syntax highlighting to <code class="language-swift">
// blocks at serve and build time. The source index.html always keeps raw
// code blocks; highlighting is applied on the fly.
function shikiPlugin() {
  let highlighter;

  return {
    name: 'shiki-highlight',
    async buildStart() {
      highlighter = await createHighlighter({
        themes: ['catppuccin-mocha'],
        langs: ['swift'],
      });
    },
    transformIndexHtml(html) {
      if (!highlighter) return html;

      return html.replace(
        /<pre><code class="language-swift">([\s\S]*?)<\/code><\/pre>/g,
        (_, code) => {
          const raw = code
            .replace(/&lt;/g, '<')
            .replace(/&gt;/g, '>')
            .replace(/&amp;/g, '&')
            .replace(/&#39;/g, "'")
            .replace(/&quot;/g, '"');

          return highlighter.codeToHtml(raw.trim(), {
            lang: 'swift',
            theme: 'catppuccin-mocha',
          });
        }
      );
    },
  };
}

export default defineConfig({
  root: '.',
  build: {
    outDir: 'dist',
  },
  publicDir: 'public',
  plugins: [shikiPlugin()],
});
