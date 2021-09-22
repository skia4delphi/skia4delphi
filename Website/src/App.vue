<template>
	<v-app app dark>
		<Topbar />

		<v-main>
			<router-view />
		</v-main>

		<Footer />
	</v-app>
</template>

<script>
	import Topbar from "@/components/Topbar.vue";
	import Footer from "@/components/Footer.vue";

	export default {
		name: "App",
		components: {
			Topbar,
			Footer,
		},
		data: () => ({
			//
		}),
	};
</script>
<style>
	html,
	body {
		background-color: #030b1f;
		font-family: "Rubik", -apple-system, BlinkMacSystemFont, Helvetica, Arial,
			sans-serif;
		-webkit-font-smoothing: antialiased;
		-moz-osx-font-smoothing: grayscale;
	}

	audio {
		display: none !important;
	}

	html,
	body {
		line-height: 1.4;
		color: #fff;
	}

	html,
	body {
		min-height: 100%;
	}

	input,
	button,
	textarea {
		font-family: inherit;
	}

	* {
		box-sizing: border-box;
		margin: 0;
		padding: 0;
		outline: 0 none;
		-webkit-tap-highlight-color: rgba(0, 0, 0, 0);
	}

	ul {
		list-style: none;
	}

	b,
	strong {
		font-weight: 500;
	}

	hr {
		background-color: rgba(255, 255, 255, 0.3);
		border: 0 none;
		display: block;
		height: 1px !important;
		margin: 2rem 0;
		width: 100%;
	}

	svg {
		display: block;
	}

	.v-application {
		font-family: "Rubik", -apple-system, BlinkMacSystemFont, Helvetica, Arial,
			sans-serif !important;
	}

	.page {
		padding-top: 80px;
	}

	.theme--dark.v-application {
		background: #151515 url("./assets/background.jpg") no-repeat center center /
			cover !important;
	}

	.wrapper {
		margin: 0 auto;
		max-width: 1200px;
		padding: 0 20px;
		width: 100%;
	}
</style>
