<template>
	<footer>
		<div class="wrapper">
			<div class="left">
				<div class="copyright">
					{{ year }} &copy; Skia4Delphi. All rights reserved.
				</div>
			</div>
			<div class="right">
				<nav>
					<a
						class="btn github"
						href="https://github.com/viniciusfbb/skia4delphi"
						title="Github"
						target="_blank"
						><span>Github</span></a
					>
				</nav>
			</div>
		</div>
	</footer>
</template>

<script>
	export default {
		data() {
			return {
				year: 0,
			};
		},
		beforeMount() {
			const date = new Date();

			this.year = date.getFullYear();
		},
	};
</script>

<style scoped>
	footer {
		background-color: #000;
		display: flex;
		flex-flow: row nowrap;
		height: 80px;
		width: 100%;
	}

	footer > .wrapper {
		align-items: center;
		display: flex;
		flex-flow: row nowrap;
		height: 100%;
	}

	footer .left {
		flex-grow: 1;
	}

	footer .btn {
		transition: opacity 0.3s;

		background: url("../assets/logo-github.svg");
		display: block;
		height: 28px;
		width: 28px;
	}

	footer .btn:hover {
		opacity: 0.5;
	}

	footer .btn span {
		display: none;
	}

	@media (max-width: 600px) {
		footer {
			height: 60px;
		}

		footer .left {
			font-size: 0.875rem;
			text-align: center;
		}

		footer .right {
			display: none;
		}
	}
</style>
