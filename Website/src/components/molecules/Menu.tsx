import styled from '@emotion/styled';
import { Text } from '@nextui-org/react';
import {Link} from 'react-router-dom';

const Menu = styled.ul`
  display: flex;
  margin: 0;
  padding: 0;
  gap: 32px;
`;

const textGradient = {
  '&:hover': {
    textGradient: '45deg, $red200 -20%, $red500 50%'
  }
};

export default () => (
  <Menu>
    <li>
      <Link to="/about">
        <Text weight="bold" css={textGradient}>About</Text>
      </Link>
    </li>
    <li>
      <a href="https://github.com/viniciusfbb/skia4delphi#summary">
        <Text weight="bold" css={textGradient}>Documentation</Text>
      </a>
    </li>
    <li>
      <a href="https://github.com/viniciusfbb/skia4delphi/releases">
        <Text weight="bold" css={textGradient}>Download</Text>
      </a>
    </li>
    <li>
      <a href="https://github.com/viniciusfbb/skia4delphi">
        <Text weight="bold" css={textGradient}>Github</Text>
      </a>
    </li>
  </Menu>
)
