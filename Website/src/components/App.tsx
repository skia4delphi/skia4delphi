import React from 'react';
import { Col, NextUIProvider } from '@nextui-org/react';
import { BrowserRouter, Route, Routes } from "react-router-dom";

import "@fontsource/rubik";
import backgroundImage from './bg.jpg';

import { theme } from '../theme';
import About from './pages/About';
import Home from './pages/Home';
import Header from './organisms/Header';
import Footer from './organisms/Footer';

const ContentCss = {
  height: '100%',
  backgroundImage: `url(${backgroundImage})`,
  backgroundSize: 'cover',
  backgroundRepeat: 'no-repeat'
}

export default function App() {
  return (
    <NextUIProvider theme={theme}>
      <BrowserRouter>
        <Col css={ContentCss}>
          <Header />
          <main>
            <Routes>
              <Route path="/" element={<Home />} />
              <Route path="/about" element={<About />} />
            </Routes>
          </main>
          <Footer />
        </Col>
      </BrowserRouter>
    </NextUIProvider>
  );
}
