import React, { useState, useEffect } from 'react';
import {
  Box,
  Typography,
  Card,
  CardContent,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Chip,
  CircularProgress,
  Alert,
  Accordion,
  AccordionSummary,
  AccordionDetails,
} from '@mui/material';
import { Description as CopybookIcon, ExpandMore } from '@mui/icons-material';
import api from '../../services/api';

interface Props {
  uploadId: string;
  programName: string;
}

const ProgramCopybookUsageView: React.FC<Props> = ({ uploadId, programName }) => {
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [copybookData, setCopybookData] = useState<any>(null);

  useEffect(() => {
    loadData();
  }, [uploadId, programName]);

  const loadData = async () => {
    setLoading(true);
    setError(null);

    try {
      const data = await api.getProgramCopybooks(uploadId, programName);
      setCopybookData(data);
      setLoading(false);
    } catch (err: any) {
      setError(err.message || 'Failed to load copybook usage');
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="300px">
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Alert severity="error" sx={{ m: 2 }}>
        {error}
      </Alert>
    );
  }

  if (!copybookData || copybookData.total_copybooks === 0) {
    return (
      <Alert severity="info" sx={{ m: 2 }}>
        No copybook usage found for program <strong>{programName}</strong>
      </Alert>
    );
  }

  return (
    <Box>
      <Card sx={{ mb: 3 }}>
        <CardContent>
          <Box display="flex" alignItems="center" mb={2}>
            <CopybookIcon sx={{ mr: 2, fontSize: 40, color: 'primary.main' }} />
            <Box>
              <Typography variant="h5">Copybook Usage</Typography>
              <Typography variant="body2" color="textSecondary">
                Program: <strong>{programName}</strong>
              </Typography>
            </Box>
          </Box>

          <Box display="flex" gap={3} mt={3}>
            <Box>
              <Typography variant="h3" color="primary">
                {copybookData.total_copybooks}
              </Typography>
              <Typography variant="caption" color="textSecondary">
                Copybooks Used
              </Typography>
            </Box>
          </Box>
        </CardContent>
      </Card>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow sx={{ backgroundColor: '#f5f5f5' }}>
              <TableCell><strong>Copybook Name</strong></TableCell>
              <TableCell><strong>Type</strong></TableCell>
              <TableCell><strong>Usage Context</strong></TableCell>
              <TableCell><strong>Line Number</strong></TableCell>
              <TableCell><strong>Data Structures</strong></TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {copybookData.copybooks.map((copybook: any, idx: number) => (
              <TableRow key={idx} hover>
                <TableCell>
                  <Typography variant="body2" fontWeight="bold">
                    {copybook.copybook_name}
                  </Typography>
                  {copybook.file_path && (
                    <Typography variant="caption" color="textSecondary" display="block">
                      {copybook.file_path.split(/[/\\]/).pop()}
                    </Typography>
                  )}
                </TableCell>
                <TableCell>
                  <Chip
                    label={copybook.copybook_type || 'COPYBOOK'}
                    size="small"
                    color="primary"
                  />
                </TableCell>
                <TableCell>
                  <Chip
                    label={copybook.usage_context || 'UNKNOWN'}
                    size="small"
                    color={
                      copybook.usage_context === 'WORKING_STORAGE'
                        ? 'success'
                        : copybook.usage_context === 'FILE_SECTION'
                        ? 'info'
                        : 'default'
                    }
                  />
                </TableCell>
                <TableCell>
                  <Chip label={`Line ${copybook.line_number || 'N/A'}`} size="small" variant="outlined" />
                </TableCell>
                <TableCell>
                  {copybook.data_structures && copybook.data_structures.length > 0 ? (
                    <Box>
                      {copybook.data_structures.slice(0, 3).map((struct: string, i: number) => (
                        <Chip
                          key={i}
                          label={struct}
                          size="small"
                          variant="outlined"
                          sx={{ mr: 0.5, mb: 0.5 }}
                        />
                      ))}
                      {copybook.data_structures.length > 3 && (
                        <Typography variant="caption" color="textSecondary">
                          +{copybook.data_structures.length - 3} more
                        </Typography>
                      )}
                    </Box>
                  ) : (
                    <Typography variant="caption" color="textSecondary">
                      None
                    </Typography>
                  )}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      {/* Detailed Information */}
      <Box mt={3}>
        <Typography variant="h6" gutterBottom>
          Copybook Details
        </Typography>
        {copybookData.copybooks.map((copybook: any, idx: number) => (
          <Accordion key={idx}>
            <AccordionSummary expandIcon={<ExpandMore />}>
              <Typography>
                <strong>{copybook.copybook_name}</strong>
                {copybook.file_path && (
                  <Typography component="span" variant="caption" color="textSecondary" sx={{ ml: 2 }}>
                    {copybook.file_path}
                  </Typography>
                )}
              </Typography>
            </AccordionSummary>
            <AccordionDetails>
              <Box>
                <Typography variant="body2" gutterBottom>
                  <strong>Usage Context:</strong> {copybook.usage_context || 'UNKNOWN'}
                </Typography>
                <Typography variant="body2" gutterBottom>
                  <strong>Line Number:</strong> {copybook.line_number || 'N/A'}
                </Typography>
                {copybook.data_structures && copybook.data_structures.length > 0 && (
                  <>
                    <Typography variant="body2" gutterBottom sx={{ mt: 2 }}>
                      <strong>Data Structures ({copybook.data_structures.length}):</strong>
                    </Typography>
                    <Box display="flex" flexWrap="wrap" gap={1}>
                      {copybook.data_structures.map((struct: string, i: number) => (
                        <Chip
                          key={i}
                          label={struct}
                          size="small"
                          variant="outlined"
                          color="primary"
                        />
                      ))}
                    </Box>
                  </>
                )}
              </Box>
            </AccordionDetails>
          </Accordion>
        ))}
      </Box>
    </Box>
  );
};

export default ProgramCopybookUsageView;
