import { Container, Row, Col } from '@nextui-org/react';
import {Link} from 'react-router-dom';
import Logo from '../../logo.svg';
import Menu from '../molecules/Menu';

export default () => (
  <Container css={{paddingTop: 16}}>
    <nav>
      <Row>
        <Col><Link to="/"><img src={Logo} alt="Skia4Delphi" /></Link></Col>
        <Col css={{display: 'flex', justifyContent: 'end'}}>
          <Menu />
        </Col>
      </Row>
    </nav>
  </Container>
);
