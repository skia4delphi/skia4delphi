<template>
	<div class="page home">
		<div class="wrapper">
			<div class="hero">
				<h1>
					Skia4Delphi is a cross-platform 2D graphics API for Delphi based on
					<a href="https://skia.org" target="_blank"
						>Google's Skia Graphics Library</a
					>
				</h1>
				<div class="actions">
					<router-link to="/about" class="btn secondary" v-ripple>
						<span class="label">Learn more</span>
						<span class="description">About Skia4Delphi</span>
					</router-link>
					<div
						class="btn primary"
						@click="
							$root.download(
								'https://api.github.com/repos/viniciusfbb/skia4delphi/releases/latest'
							)
						"
						v-ripple
					>
						<span class="label">Download</span>
						<span class="description">Latest version</span>
					</div>
				</div>
			</div>
		</div>
	</div>
</template>

<script>
	export default {
		name: "Home",
	};
</script>

<style scoped>
	.home {
		display: flex;
		height: 100%;
	}

	.hero {
		max-width: 740px;
		margin: 120px auto 0;
		padding-bottom: 100px;
		text-align: center;
		width: 100%;
	}

	.hero h1 {
		font-size: 2.25rem;
		font-weight: 400;
		line-height: normal;
		margin-bottom: 1.25rem;
	}

	.hero h1 a {
		transition: all 0.3s;
		color: transparent;
		text-decoration: none;
		background: linear-gradient(0deg, #f12657, #f8a515);
		background-clip: text;
		-webkit-box-decoration-break: clone;
	}

	.hero h1 a:hover {
		transform: scale(1.1);
		text-decoration: underline;
	}

	.hero .actions {
		display: flex;
		flex-flow: row nowrap;
		justify-content: center;
	}

	.btn {
		transition: transform 0.3s, background 0.3s;

		align-items: center;
		border-radius: 8px;
		color: #fff;
		cursor: pointer;
		display: inline-flex;
		flex-flow: column wrap;
		line-height: normal;
		height: 64px;
		justify-content: center;
		margin: 0 8px;
		padding: 0 24px;
		text-decoration: none;
		user-select: none;
	}

	.btn:active {
		transform: scale(0.95);
	}

	.btn .label {
		font-size: 1.125rem;
		font-weight: 500;
	}

	.btn .description {
		font-size: 0.875rem;
		font-weight: 500;
		color: rgba(255, 255, 255, 0.75);
	}

	@media (max-width: 600px) {
		.hero {
			margin-top: 80px;
		}

		.hero h1 {
			font-size: 1.5rem;
		}

		.hero .actions {
			display: flex;
			flex-flow: column wrap;
			justify-content: center;
		}

		.btn {
			margin: 8px 0;
		}
	}
</style>
