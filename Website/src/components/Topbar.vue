<template>
	<header class="topbar">
		<div class="wrapper">
			<div class="hamb" :class="{ opened: opened }" @click="open">
				<div class="tile"></div>
			</div>
			<div class="left">
				<router-link to="/">
					<div class="logotype" title="Go to Homepage"></div>
				</router-link>
			</div>
			<nav :class="{ opened: opened }">
				<div class="logotype" @click="opened = !1" title="Go to Homepage"></div>
				<ul>
					<li class="mobile-only" @click="opened = !1">
						<router-link to="/" title="Homepage">Home</router-link>
					</li>
					<li @click="opened = !1">
						<router-link to="/about" title="About Skia4Delphi"
							>About</router-link
						>
					</li>
					<li>
						<a
							href="javascript:void(0);"
							@click="
								$root.download(
									'https://api.github.com/repos/viniciusfbb/skia4delphi/releases/latest'
								)
							"
							title="Download latest version"
							>Download</a
						>
					</li>
					<li>
						<a
							href="https://github.com/viniciusfbb/skia4delphi"
							target="_blank"
							title="Github"
							>Github</a
						>
					</li>
				</ul>
			</nav>
		</div>
	</header>
</template>

<script>
	export default {
		name: "topbar",
		data() {
			return {
				opened: !1,
			};
		},
		watch: {
			$route() {
				this.opened = !1;
			},
		},
		methods: {
			open() {
				this.opened = !this.opened;
			},
		},
	};
</script>

<style scoped>
	.topbar {
		display: flex;
		flex-flow: row nowrap;
		height: 80px;
		left: 0;
		top: 0;
		position: fixed;
		width: 100%;
		z-index: 100;
	}

	.topbar > .wrapper {
		display: flex;
		flex-flow: row nowrap;
		height: 100%;
	}

	.topbar .left {
		align-items: center;
		flex-grow: 1;
		display: inline-flex;
	}

	.topbar .left,
	.topbar nav {
		height: 100%;
	}

	.topbar .logotype {
		background: url("../assets/logo.svg") no-repeat center center / contain;
		display: inline-block;
		height: 40px;
		width: 82px;
	}

	.topbar nav {
		align-items: center;
		display: inline-flex;
	}

	.topbar nav ul {
		display: flex;
		flex-flow: row nowrap;
		padding-left: 0 !important;
	}

	.topbar nav ul li {
		margin: 0 1.5rem;
	}

	.topbar nav ul li:first-child {
		margin-left: 0;
	}

	.topbar nav ul li:last-child {
		margin-right: 0;
	}

	.topbar nav ul li a {
		transition: color 0.3s;

		color: #fff;
		font-size: 1.125rem;
		font-weight: 500;
		text-decoration: none;
	}

	.topbar nav ul li a:hover {
		color: rgba(255, 255, 255, 0.75);
	}

	.topbar nav .logotype {
		display: none;
	}

	.mobile-only {
		display: none;
	}

	@media (max-width: 600px) {
		.hamb {
			align-items: center;
			cursor: pointer;
			height: 36px;
			display: flex;
			flex-flow: row nowrap;
			position: fixed;
			justify-content: center;
			right: 15px;
			top: 15px;
			width: 36px;
			z-index: 101;
		}

		.hamb .tile,
		.hamb:after,
		.hamb:before {
			transition: transform 0.3s, opacity 0.3s;

			background-color: #fff;
			height: 2px;
			width: 24px;
		}

		.hamb:after,
		.hamb:before {
			content: "";
			bottom: 0;
			left: 0;
			margin: auto;
			position: absolute;
			right: 0;
			top: 0;
		}

		.hamb.opened .tile {
			transform: scale(0);
			opacity: 0;
		}

		.hamb:after {
			transform: translate3d(0, 6px, 0);
		}

		.hamb:before {
			transform: translate3d(0, -6px, 0);
		}

		.hamb.opened:after {
			transform: translate3d(0, 0, 0) rotate(45deg);
		}

		.hamb.opened:before {
			transform: translate3d(0, 0, 0) rotate(-45deg);
		}

		.mobile-only {
			display: block;
		}

		.topbar nav {
			transition: visibility 0.3s 0.3s, opacity 0.3s;

			align-items: center;
			display: flex;
			flex-flow: column wrap;
			background-color: rgba(0, 0, 0, 0.9);
			height: 100%;
			left: 0;
			text-align: center;
			top: 0;
			justify-content: flex-start;
			opacity: 0;
			padding: 20px 0;
			position: fixed;
			pointer-events: none;
			visibility: hidden;
			width: 100%;
			z-index: 100;
		}

		.topbar nav.opened {
			transition: visibility 0.3s, opacity 0.3s;

			opacity: 1;
			pointer-events: all;
			visibility: visible;
		}

		.topbar .logotype {
			background: url("../assets/logo.svg") no-repeat center center / contain;
			display: inline-block !important;
			height: 40px;
			width: 82px;
		}

		.topbar nav ul {
			display: flex;
			flex-flow: column wrap;
			padding-left: 0 !important;
		}

		.topbar nav ul li {
			margin: 1.5rem 0 !important;
		}

		.topbar nav ul li a {
			transition: color 0.3s;

			color: #fff;
			font-size: 1.5rem;
			font-weight: 500;
			text-decoration: none;
		}
	}
</style>
