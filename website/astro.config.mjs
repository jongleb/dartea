// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

export default defineConfig({
	site: 'https://jongleb.github.io',
	base: '/dartea',
	integrations: [
		starlight({
			title: 'dartea',
			description: 'An Elm compiler. Modern JavaScript output. No virtual DOM.',
			logo: {
				light: './src/assets/lockup-light.svg',
				dark: './src/assets/lockup-dark.svg',
				replacesTitle: true,
			},
			head: [
				{ tag: 'meta', attrs: { property: 'og:image', content: 'https://jongleb.github.io/dartea/brand/dartea-card.png' } },
				{ tag: 'meta', attrs: { name: 'twitter:card', content: 'summary_large_image' } },
				{ tag: 'meta', attrs: { name: 'twitter:image', content: 'https://jongleb.github.io/dartea/brand/dartea-card.png' } },
				{ tag: 'link', attrs: { rel: 'apple-touch-icon', href: '/dartea/brand/apple-touch-icon.png' } },
			],
			favicon: '/favicon.svg',
			customCss: ['@fontsource-variable/inter', '@fontsource-variable/jetbrains-mono', './src/styles/theme.css'],
			social: [
				{ icon: 'github', label: 'GitHub', href: 'https://github.com/jongleb/dartea' },
				{ icon: 'npm', label: 'npm', href: 'https://www.npmjs.com/package/@dartea/cli' },
			],
			editLink: { baseUrl: 'https://github.com/jongleb/dartea/edit/main/website/' },
			lastUpdated: false,
			expressiveCode: {
				themes: ['github-dark-dimmed', 'github-light'],
				styleOverrides: { borderRadius: '0.6rem' },
			},
			sidebar: [
				{
					label: 'Start here',
					items: [
						{ label: 'Getting started', slug: 'guides/getting-started' },
						{ label: 'How it works', slug: 'guides/how-it-works' },
					],
				},
				{
					label: 'Guides',
					items: [
						{ label: 'Packages', slug: 'guides/packages' },
						{ label: 'Coming from Elm', slug: 'guides/coming-from-elm' },
						{ label: 'Benchmarks', slug: 'guides/benchmarks' },
					],
				},
				{
					label: 'Reference',
					items: [
						{ label: 'Command line', slug: 'reference/cli' },
						{ label: 'dartea.json', slug: 'reference/dartea-json' },
					],
				},
			],
		}),
	],
});
