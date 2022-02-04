import { createTheme } from "@nextui-org/react"

export const theme = createTheme({
  type: 'dark',
  theme: {
    colors: {
      primary: '$red500',
      secondary: '$gray500',
    },
    fonts: {
      sans: "Rubik, -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto'",
    }
  }
});
