import { Container, Row, Col, Text } from '@nextui-org/react';
import {Link} from 'react-router-dom';
import { IoLogoGithub } from 'react-icons/io5';
import styled from '@emotion/styled';

const Footer = styled.footer`
  background-color: #000000;
`;

export default () => (
  <Footer>
    <Container css={{paddingTop: 16}}>
        <Row>
          <Col><Text>{new Date().getUTCFullYear()} © Skia4Delphi. All rights reserved.</Text></Col>
          <Col css={{
            display: 'flex',
            justifyContent: 'end',
            fontSize: 30,
            svg: {color: 'white'}
          }}>
            <a href="https://github.com/viniciusfbb/skia4delphi">
              <IoLogoGithub />
            </a>
          </Col>
        </Row>
    </Container>
  </Footer>
);
