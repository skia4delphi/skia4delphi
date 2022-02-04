import styled from "@emotion/styled";
import { Button, Col, Row, Text } from "@nextui-org/react";
import { Link } from "react-router-dom";

const Wrapper = styled.div`
  height: calc(100vh - 130px);
  align-items: center;
  justify-content: center;
  display: flex;
  text-align: center;
`;

export default () => (
  <Wrapper>
    <Col>
      <Text h1>
        Skia4Delphi is a cross-platform 2D graphics<br />
        API for Delphi based on<br />
      </Text>
      <Text h1 css={{textGradient: '90deg, $red500 0%, $yellow500 80%'}}>Google's Skia Graphics Library</Text>

      <Row css={{justifyContent: 'center', display: 'flex', paddingTop: 16, gap: 16}}>
        <Link to="/about">
          <Button css={{padding: '$10'}} color="secondary">
            <Col>
              <Text span css={{fontSize: 18, fontWeight: '$bold', display: 'block', height: 16}}>Learn more</Text>
              <Text small>About Skia4Delphi</Text>
            </Col>
          </Button>
        </Link>
        <Button
          css={{padding: '$10'}}
          color="primary"
          onClick={() => window.location.href = 'https://github.com/viniciusfbb/skia4delphi/releases/latest'}>
          <Col>
            <Text span css={{fontSize: 18, fontWeight: '$bold', display: 'block', height: 16}}>Download</Text>
            <Text small>Latest version</Text>
          </Col>
        </Button>
      </Row>
    </Col>
  </Wrapper>
);
